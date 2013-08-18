///////////////////////////////////////////////////////////////////////////////
/// @file Z80.js
///
/// @brief Emulator for the Zilog Z80 microprocessor
///
/// @author Matthew Howell
///
/// @remarks
///  This module is a simple, straightforward instruction interpreter.
///   There is no fancy dynamic recompilation or cycle-accurate emulation.
///   The author believes that this should be sufficient for any emulator that
///   would be feasible to write in JavaScript anyway.
///  The code and the comments in this file assume that the reader is familiar
///   with the Z80 architecture. If you're not, here are some references I use:
///  http://clrhome.org/table/ - Z80 instruction set tables
///  http://www.zilog.com/docs/z80/um0080.pdf - The official manual
///  http://www.myquest.nl/z80undocumented/z80-documented-v0.91.pdf
///   - The Undocumented Z80, Documented
///
/// @copyright (c) 2013 Matthew Howell
///  This code is released under the MIT license,
///  a copy of which is available in the associated README.md file,
///  or at http://opensource.org/licenses/MIT
///////////////////////////////////////////////////////////////////////////////

"use strict";

///////////////////////////////////////////////////////////////////////////////
/// We'll begin with the object constructor and the public API functions.
///////////////////////////////////////////////////////////////////////////////
function Z80(core)
{
   // The argument to this constructor should be an object containing 4 functions:
   // mem_read(address) should return the byte at the given memory address,
   // mem_write(address, value) should write the given value to the given memory address,
   // io_read(port) should read a return a byte read from the given I/O port,
   // io_write(port, value) should write the given byte to the given I/O port.
   // If any of those functions is missing, this module cannot run.
   if (!core || (typeof core.mem_read !== "function") || (typeof core.mem_write !== "function") ||
                (typeof core.io_read !== "function")  || (typeof core.io_write !== "function"))
      throw("Z80: Core object is missing required functions.");
   
   if (this === window)
      throw("Z80: This function is a constructor; call it using operator new.");

   // Obviously we'll be needing the core object's functions again.
   this.core = core;
   
   // All right, let's initialize the registers.
   // First, the standard 8080 registers.
   this.a = 0x00;
   this.b = 0x00;
   this.c = 0x00;
   this.d = 0x00;
   this.e = 0x00;
   this.h = 0x00;
   this.l = 0x00;
   // Now the special Z80 copies of the 8080 registers
   //  (the ones used for the SWAP instruction and such).
   this.a_prime = 0x00;
   this.b_prime = 0x00;
   this.c_prime = 0x00;
   this.d_prime = 0x00;
   this.e_prime = 0x00;
   this.h_prime = 0x00;
   this.l_prime = 0x00;
   // And now the Z80 index registers.
   this.ix = 0x0000;
   this.iy = 0x0000;
   // Then the "utility" registers: the interrupt vector,
   //  the memory refresh, the stack pointer, and the program counter.
   this.i = 0x00;
   this.r = 0x00;
   this.sp = 0xdff0;
   this.pc = 0x0000;
   // We don't keep an F register for the flags,
   //  because most of the time we're only accessing a single flag,
   //  so we optimize for that case and use utility functions
   //  for the rarer occasions when we need to access the whole register.
   this.flags = {S:0, Z:0, Y:0, H:0, X:0, P:0, N:0, C:0};
   this.flags_prime = {S:0, Z:0, Y:0, H:0, X:0, P:0, N:0, C:0};
   // And finally we have the interrupt mode and flip-flop registers.
   this.imode = 0;
   this.iff1 = 0;
   this.iff2 = 0;
   
   // These are all specific to this implementation, not Z80 features.
   // Keep track of whether we've had a HALT instruction called.
   this.halted = false;
   // EI and DI wait one instruction before they take effect;
   //  these flags tell us when we're in that wait state.
   this.do_delayed_di = false;
   this.do_delayed_ei = false;
   // This tracks the number of cycles spent in a single instruction run,
   //  including processing any prefixes and handling interrupts.
   this.cycle_counter = 0;
   
   // There's tons of stuff in this object,
   //  but only these three functions are the public API.
   return {
      reset : this.reset.bind(this),
      run_instruction : this.run_instruction.bind(this),
      interrupt : this.interrupt.bind(this)
   };
}

///////////////////////////////////////////////////////////////////////////////
/// @public reset
///
/// @brief Re-initialize the processor as if a reset or power on had occured
///////////////////////////////////////////////////////////////////////////////
Z80.prototype.reset = function()
{
   // These registers are the ones that have predictable states
   //  immediately following a power-on or a reset.
   // The others are left alone, because their states are unpredictable.
   this.sp = 0xdff0;
   this.pc = 0x0000;
   this.a = 0x00;
   this.r = 0x00;
   this.set_flags_register(0);
   // Start up with interrupts disabled.
   this.imode = 0;
   this.iff1 = 0;
   this.iff2 = 0;
   // Don't start halted or in a delayed DI or EI.
   this.halted = false;
   this.do_delayed_di = false;
   this.do_delayed_ei = false;
   // Obviously we've not used any cycles yet.
   this.cycle_counter = 0;
};

///////////////////////////////////////////////////////////////////////////////
/// @public run_instruction
///
/// @brief Runs a single instruction
///
/// @return The number of T cycles the instruction took to run,
///          plus any time that went into handling interrupts that fired
///          while this instruction was executing
///////////////////////////////////////////////////////////////////////////////
Z80.prototype.run_instruction = function()
{
   if (!this.halted)
   {
      // If the previous instruction was a DI or an EI,
      //  we'll need to disable or enable interrupts
      //  after whatever instruction we're about to run is finished.
      var doing_delayed_di = false, doing_delayed_ei = false;
      if (this.do_delayed_di)
      {
         this.do_delayed_di = false;
         doing_delayed_di = true;
      }
      else if (this.do_delayed_ei)
      {
         this.do_delayed_ei = false;
         doing_delayed_ei = true;
      }

      // R is incremented at the start of every instruction cycle,
      //  before the instruction actually runs.
      // The high bit of R is not affected by this increment,
      //  it can only be changed using the LD R, A instruction.
      this.r = (this.r & 0x80) | (((this.r & 0x7f) + 1) & 0x7f);
      
      // Read the byte at the PC and run the instruction it encodes.
      var opcode = this.core.mem_read(this.pc);
      this.decode_instruction(opcode);
      this.pc = (this.pc + 1) & 0xffff;
      
      // Actually do the delayed interrupt disable/enable if we have one.
      if (doing_delayed_di)
      {
         this.iff1 = 0;
         this.iff2 = 0;
      }
      else if (doing_delayed_ei)
      {
         this.iff1 = 1;
         this.iff2 = 1;
      }
      
      // And finally clear out the cycle counter for the next instruction
      //  before returning it to the emulator core.
      var retval = this.cycle_counter;
      this.cycle_counter = 0;
      return retval;
   }
   else
   {
      // While we're halted, claim that we spent a cycle doing nothing,
      //  so that the rest of the emulator can still proceed.
      return 1;
   }
};

///////////////////////////////////////////////////////////////////////////////
/// @public interrupt
///
/// @brief Simulates pulsing the processor's INT (or NMI) pin
///
/// @param non_maskable - true if this is a non-maskable interrupt
/// @param data - the value to be placed on the data bus, if needed
///////////////////////////////////////////////////////////////////////////////
Z80.prototype.interrupt = function(non_maskable, data)
{
   if (non_maskable)
   {
      // The high bit of R is not affected by this increment,
      //  it can only be changed using the LD R, A instruction.
      this.r = (this.r & 0x80) | (((this.r & 0x7f) + 1) & 0x7f);
      // Non-maskable interrupts are always handled the same way;
      //  clear IFF1 and then do a CALL 0x0066.
      // Also, all interrupts reset the HALT state.
      this.halted = false;
      this.iff2 = this.iff1;
      this.iff1 = 0;
      this.push_word(this.pc);
      this.pc = 0x66;
      this.cycle_counter += 11;
   }
   else if (this.iff1)
   {
      // The high bit of R is not affected by this increment,
      //  it can only be changed using the LD R, A instruction.
      this.r = (this.r & 0x80) | (((this.r & 0x7f) + 1) & 0x7f);
   
      this.halted = false;
      this.iff1 = 0;
      this.iff2 = 0;
   
      if (this.imode === 0)
      {
         // In the 8080-compatible interrupt mode,
         //  decode the content of the data bus as an instruction and run it.
         this.decode_instruction(data);
         this.cycle_counter += 2;
      }
      else if (this.imode === 1)
      {
         // Mode 1 is always just RST 0x38.
         this.push_word(this.pc);
         this.pc = 0x38;
         this.cycle_counter += 13;
      }
      else if (this.imode === 2)
      {
         // Mode 2 uses the value on the data bus as in index
         //  into the vector table pointer to by the I register.
         this.push_word(this.pc);
         // The Z80 manual says that this address must be 2-byte aligned,
         //  but it doesn't appear that this is actually the case on the hardware,
         //  so we don't attempt to enforce that here.
         var vector_address = ((this.i << 8) | data);
         this.pc = this.core.read_mem_byte(vector_address) | 
                   (this.core.read_mem_byte((vector_address + 1) & 0xffff) << 8);
         
         this.cycle_counter += 19;
      }
   }
};

///////////////////////////////////////////////////////////////////////////////
/// The public API functions end here.
///
/// What begins here are just general utility functions, used variously.
///////////////////////////////////////////////////////////////////////////////
Z80.prototype.decode_instruction = function(opcode)
{
   // The register-to-register loads and ALU instructions
   //  are all so uniform that we can decode them directly
   //  instead of going into the instruction array for them.
   // This function gets the operand for all of these instructions.
   var get_operand = function(opcode)
   {
      return ((opcode & 0x07) === 0) ? this.b :
             ((opcode & 0x07) === 1) ? this.c :
             ((opcode & 0x07) === 2) ? this.d :
             ((opcode & 0x07) === 3) ? this.e :
             ((opcode & 0x07) === 4) ? this.h :
             ((opcode & 0x07) === 5) ? this.l :
             ((opcode & 0x07) === 6) ? this.core.mem_read(this.l | (this.h << 8)) : this.a;
   };

   // Handle HALT right up front, because it fouls up our LD decoding
   //  by falling where LD (HL), (HL) ought to be.
   if (opcode === 0x76)
   {
      this.halted = true;
      this.iff1 = 1;
      this.iff2 = 1;
   }
   else if ((opcode >= 0x40) && (opcode < 0x80))
   {
      // This entire range is all 8-bit register loads.
      // Get the operand and assign it to the correct destination.
      var operand = get_operand.call(this, opcode);
         
      if (((opcode & 0x38) >>> 3) === 0)
         this.b = operand;
      else if (((opcode & 0x38) >>> 3) === 1)
         this.c = operand;
      else if (((opcode & 0x38) >>> 3) === 2)
         this.d = operand;
      else if (((opcode & 0x38) >>> 3) === 3)
         this.e = operand;
      else if (((opcode & 0x38) >>> 3) === 4)
         this.h = operand;
      else if (((opcode & 0x38) >>> 3) === 5)
         this.l = operand;
      else if (((opcode & 0x38) >>> 3) === 6)
         this.core.mem_write(this.l | (this.h << 8), operand);
      else if (((opcode & 0x38) >>> 3) === 7)
         this.a = operand;
   }
   else if ((opcode >= 0x80) && (opcode < 0xc0))
   {
      // These are the 8-bit register ALU instructions.
      // We'll get the operand and then use this "jump table"
      //  to call the correct utility function for the instruction.
      var operand = get_operand.call(this, opcode),
          op_array = [this.do_add, this.do_adc, this.do_sub, this.do_sbc,
                      this.do_and, this.do_xor, this.do_or, this.do_cp];
      
      op_array[(opcode & 0x38) >>> 3].call(this, operand);
   }
   else
   {
      // This is one of the less formulaic instructions;
      //  we'll get the specific function for it from our array.
      var func = this.instructions[opcode].bind(this);
      func();
   }
   
   // Update the cycle counter with however many cycles
   //  the base instruction took.
   // If this was a prefixed instruction, then
   //  the prefix handler has added its extra cycles already.
   this.cycle_counter += this.cycle_counts[opcode];
};

Z80.prototype.get_signed_offset_byte = function(value)
{
   // This function requires some explanation.
   // We just use JavaScript Number variables for our registers,
   //  not like a typed array or anything.
   // That means that, when we have a byte value that's supposed
   //  to represent a signed offset, the value we actually see
   //  isn't signed at all, it's just a small integer.
   // So, this function converts that byte into something JavaScript
   //  will recognize as signed, so we can easily do arithmetic with it.
   // First, we clamp the value to a single byte, just in case.
   value &= 0xff;
   // We don't have to do anything if the value is positive.
   if (value & 0x80)
   {
      // But if the value is negative, we need to manually un-two's-compliment it.
      // I'm going to assume you can figure out what I meant by that,
      //  because I don't know how else to explain it.
      // We could also just do value |= 0xffffff00, but I prefer
      //  not caring how many bits are in the integer representation
      //  of a JavaScript number in the currently running browser.
      value = -((0xff & ~value) + 1);
   }
   return value;
};

Z80.prototype.get_flags_register = function()
{
   // We need the whole F register for some reason.
   //  probably a PUSH AF instruction,
   //  so make the F register out of our separate flags.
   return (this.flags.S << 7) |
          (this.flags.Z << 6) |
          (this.flags.Y << 5) |
          (this.flags.H << 4) |
          (this.flags.X << 3) |
          (this.flags.P << 2) |
          (this.flags.N << 1) |
          (this.flags.C);
};

Z80.prototype.get_flags_prime = function()
{
   // This is the same as the above for the F' register.
   return (this.flags_prime.S << 7) |
          (this.flags_prime.Z << 6) |
          (this.flags_prime.Y << 5) |
          (this.flags_prime.H << 4) |
          (this.flags_prime.X << 3) |
          (this.flags_prime.P << 2) |
          (this.flags_prime.N << 1) |
          (this.flags_prime.C);
};

Z80.prototype.set_flags_register = function(operand)
{
   // We need to set the F register, probably for a POP AF,
   //  so break out the given value into our separate flags.
   this.flags.S = (operand & 0x80) >>> 7;
   this.flags.Z = (operand & 0x40) >>> 6;
   this.flags.Y = (operand & 0x20) >>> 5;
   this.flags.H = (operand & 0x10) >>> 4;
   this.flags.X = (operand & 0x08) >>> 3;
   this.flags.P = (operand & 0x04) >>> 2;
   this.flags.N = (operand & 0x02) >>> 1;
   this.flags.C = (operand & 0x01);
};

Z80.prototype.set_flags_prime = function(operand)
{
   // Again, this is the same as the above for F'.
   this.flags_prime.S = (operand & 0x80) >>> 7;
   this.flags_prime.Z = (operand & 0x40) >>> 6;
   this.flags_prime.Y = (operand & 0x20) >>> 5;
   this.flags_prime.H = (operand & 0x10) >>> 4;
   this.flags_prime.X = (operand & 0x08) >>> 3;
   this.flags_prime.P = (operand & 0x04) >>> 2;
   this.flags_prime.N = (operand & 0x02) >>> 1;
   this.flags_prime.C = (operand & 0x01);
};

Z80.prototype.update_xy_flags = function(result)
{
   // Most of the time, the undocumented flags
   //  (sometimes called X and Y, or 3 and 5),
   //  take their values from the corresponding bits
   //  of the result of the instruction,
   //  or from some other related value.
   // This is a utility function to set those flags based on those bits.
   this.flags.Y = (result & 0x20) >>> 5;
   this.flags.X = (result & 0x08) >>> 3;
};

Z80.prototype.get_parity = function(value)
{
   // We could try to actually calculate the parity every time,
   //  but why calculate what you can pre-calculate?
   var parity_bits = [
      1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1,
      0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 
      0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 
      1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 
      0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 
      1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 
      1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 
      0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 
      0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 
      1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 
      1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 
      0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 
      1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 
      0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 
      0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 
      1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1
   ];
   return parity_bits[value];
};

Z80.prototype.push_word = function(operand)
{
   // Pretty obvious what this function does; given a 16-bit value,
   //  decrement the stack pointer, write the high byte to the new
   //  stack pointer location, then repeat for the low byte.
   this.sp = (this.sp - 1) & 0xffff;
   this.core.mem_write(this.sp, (operand & 0xff00) >>> 8);
   this.sp = (this.sp - 1) & 0xffff;
   this.core.mem_write(this.sp, operand & 0x00ff);
};

Z80.prototype.pop_word = function()
{
   // Again, not complicated; read a byte off the top of the stack,
   //  increment the stack pointer, rinse and repeat.
   var retval = this.core.mem_read(this.sp) & 0xff;
   this.sp = (this.sp + 1) & 0xffff;
   retval |= this.core.mem_read(this.sp) << 8;
   this.sp = (this.sp + 1) & 0xffff;
   return retval;
};

///////////////////////////////////////////////////////////////////////////////
/// Now, the way most instructions work in this emulator is that they set up
///  their operands according to their addressing mode, and then they call a
///  utility function that handles all variations of that instruction.
/// Those utility functions begin here.
///////////////////////////////////////////////////////////////////////////////
Z80.prototype.do_conditional_absolute_jump = function(condition)
{
   // This function implements the JP [condition],nn instructions.
   if (condition)
   {
      // We're taking this jump, so write the new PC,
      //  and then decrement the thing we just wrote,
      //  because the instruction decoder increments the PC
      //  unconditionally at the end of every instruction
      //  and we need to counteract that so we end up at the jump target.
      this.pc =  this.core.mem_read((this.pc + 1) & 0xffff) |
                (this.core.mem_read((this.pc + 2) & 0xffff) << 8);
      this.pc = (this.pc - 1) & 0xffff;
   }
   else
   {
      // We're not taking this jump, just move the PC past the operand.
      this.pc = (this.pc + 2) & 0xffff;
   }
};

Z80.prototype.do_conditional_relative_jump = function(condition)
{
   // This function implements the JR [condition],n instructions.
   if (condition)
   {
      // We need a few more cycles to actually take the jump.
      this.cycle_counter += 5;
      // Calculate the offset specified by our operand.
      var offset = this.get_signed_offset_byte(this.core.mem_read((this.pc + 1) & 0xffff));
      // Add the offset to the PC, also skipping past this instruction.
      this.pc = (this.pc + offset + 1) & 0xffff;
   }
   else
   {
      // No jump happening, just skip the operand.
      this.pc = (this.pc + 1) & 0xffff;
   }
};

Z80.prototype.do_conditional_call = function(condition)
{
   // This function is the CALL [condition],nn instructions.
   // If you've seen the previous functions, you know this drill.
   if (condition)
   {
      this.cycle_counter += 7;
      this.push_word((this.pc + 3) & 0xffff);
      this.pc =  this.core.mem_read((this.pc + 1) & 0xffff) |
                (this.core.mem_read((this.pc + 2) & 0xffff) << 8);
      this.pc = (this.pc - 1) & 0xffff;
   }
   else
   {
      this.pc = (this.pc + 2) & 0xffff;
   }
};

Z80.prototype.do_conditional_return = function(condition)
{
   if (condition)
   {
      this.cycle_counter += 6;
      this.pc = (this.pop_word() - 1) & 0xffff;
   }
};

Z80.prototype.do_reset = function(address)
{
   // The RST [address] instructions go through here.
   this.push_word((this.pc + 1) & 0xffff);
   this.pc = (address - 1) & 0xffff;
};

Z80.prototype.do_add = function(operand)
{
   // This is the ADD A, [operand] instructions.
   // We'll do the literal addition, which includes any overflow,
   //  so that we can more easily figure out whether we had
   //  an overflow or a carry and set the flags accordingly.
   var result = this.a + operand;
   
   // The great majority of the work for the arithmetic instructions
   //  turns out to be setting the flags rather than the actual operation.
   this.flags.S = (result & 0x80) ? 1 : 0;
   this.flags.Z = !(result & 0xff) ? 1 : 0;
   this.flags.H = (((operand & 0x0f) + (this.a & 0x0f)) & 0x10) ? 1 : 0;
   // An overflow has happened if the sign bits of the accumulator and the operand
   //  don't match the sign bit of the result value.
   this.flags.P = ((this.a & 0x80) === (operand & 0x80)) && ((this.a & 0x80) !== (result & 0x80)) ? 1 : 0;
   this.flags.N = 0;
   this.flags.C = (result & 0x100) ? 1 : 0;
   
   this.a = result & 0xff;
   this.update_xy_flags(this.a);
};

Z80.prototype.do_adc = function(operand)
{
   var result = this.a + operand + this.flags.C;
   
   this.flags.S = (result & 0x80) ? 1 : 0;
   this.flags.Z = !(result & 0xff) ? 1 : 0;
   this.flags.H = (((operand & 0x0f) + (this.a & 0x0f) + this.flags.C) & 0x10) ? 1 : 0;
   this.flags.P = ((this.a & 0x80) === (operand & 0x80)) && ((this.a & 0x80) !== (result & 0x80)) ? 1 : 0;
   this.flags.N = 0;
   this.flags.C = (result & 0x100) ? 1 : 0;
   
   this.a = result & 0xff;
   this.update_xy_flags(this.a);
};

Z80.prototype.do_sub = function(operand)
{
   var result = this.a - operand;
   
   this.flags.S = (result & 0x80) ? 1 : 0;
   this.flags.Z = !(result & 0xff) ? 1 : 0;
   this.flags.H = (((this.a & 0x0f) - (operand & 0x0f)) & 0x10) ? 1 : 0;
   this.flags.P = ((this.a & 0x80) !== (operand & 0x80)) && ((this.a & 0x80) !== (result & 0x80)) ? 1 : 0;
   this.flags.N = 1;
   this.flags.C = (result & 0x100) ? 1 : 0;
   
   this.a = result & 0xff;
   this.update_xy_flags(this.a);
};

Z80.prototype.do_sbc = function(operand)
{
   var result = this.a - operand - this.flags.C;
   
   this.flags.S = (result & 0x80) ? 1 : 0;
   this.flags.Z = !(result & 0xff) ? 1 : 0;
   this.flags.H = (((this.a & 0x0f) - (operand & 0x0f) - this.flags.C) & 0x10) ? 1 : 0;
   this.flags.P = ((this.a & 0x80) !== (operand & 0x80)) && ((this.a & 0x80) !== (result & 0x80)) ? 1 : 0;
   this.flags.N = 1;
   this.flags.C = (result & 0x100) ? 1 : 0;
   
   this.a = result & 0xff;
   this.update_xy_flags(this.a);
};

Z80.prototype.do_cp = function(operand)
{
   // A compare instruction is just a subtraction that doesn't save the value,
   //  so we implement it as... a subtraction that doesn't save the value.
   var temp = this.a;
   this.do_sub(operand);
   this.a = temp;
   // Since this instruction has no "result" value, the undocumented flags
   //  are set based on the operand instead.
   this.update_xy_flags(operand);
};

Z80.prototype.do_and = function(operand)
{
   // The logic instructions are all pretty straightforward.
   this.a &= operand & 0xff;
   this.flags.S = (this.a & 0x80) ? 1 : 0;
   this.flags.Z = !this.a ? 1 : 0;
   this.flags.H = 1;
   this.flags.P = this.get_parity(this.a);
   this.flags.N = 0;
   this.flags.C = 0;
   this.update_xy_flags(this.a);
};

Z80.prototype.do_or = function(operand)
{
   this.a = (operand | this.a) & 0xff;
   this.flags.S = (this.a & 0x80) ? 1 : 0;
   this.flags.Z = !this.a ? 1 : 0;
   this.flags.H = 0;
   this.flags.P = this.get_parity(this.a);
   this.flags.N = 0;
   this.flags.C = 0;
   this.update_xy_flags(this.a);
};

Z80.prototype.do_xor = function(operand)
{
   this.a = (operand ^ this.a) & 0xff;
   this.flags.S = (this.a & 0x80) ? 1 : 0;
   this.flags.Z = !this.a ? 1 : 0;
   this.flags.H = 0;
   this.flags.P = this.get_parity(this.a);
   this.flags.N = 0;
   this.flags.C = 0;
   this.update_xy_flags(this.a);
};

Z80.prototype.do_inc = function(operand)
{
   var result = operand + 1;
   
   this.flags.S = (result & 0x80) ? 1 : 0;
   this.flags.Z = !(result & 0xff) ? 1 : 0;
   this.flags.H = ((operand & 0x0f) === 0x0f) ? 1 : 0;
   // It's a good deal easier to detect overflow for an increment/decrement.
   this.flags.P = (operand === 0x7f) ? 1 : 0;
   this.flags.N = 0;
   
   result &= 0xff;
   this.update_xy_flags(result);
   
   return result;
};

Z80.prototype.do_dec = function(operand)
{
   var result = operand - 1;
   
   this.flags.S = (result & 0x80) ? 1 : 0;
   this.flags.Z = !(result & 0xff) ? 1 : 0;
   this.flags.H = ((operand & 0x0f) === 0x00) ? 1 : 0;
   this.flags.P = (operand === 0x80) ? 1 : 0;
   this.flags.N = 1;
   
   result &= 0xff;
   this.update_xy_flags(result);
   
   return result;
};

Z80.prototype.do_hl_add = function(operand)
{
   // The HL arithmetic instructions are the same as the A ones,
   //  just with twice as many bits happening.
   var hl = this.l | (this.h << 8), result = hl + operand;
   
   this.flags.N = 0;
   this.flags.C = (result & 0x10000) ? 1 : 0;
   this.flags.H = (((hl & 0x0fff) + (operand & 0x0fff)) & 0x1000) ? 1 : 0;
   
   this.l = result & 0xff;
   this.h = (result & 0xff00) >>> 8;

   this.update_xy_flags(this.h);
};

Z80.prototype.do_hl_adc = function(operand)
{
   operand += this.flags.C;
   var hl = this.l | (this.h << 8), result = hl + operand;
   
   this.flags.S = (result & 0x8000) ? 1 : 0;
   this.flags.Z = !(result & 0xffff) ? 1 : 0;
   this.flags.H = (((hl & 0x0fff) + (operand & 0x0fff)) & 0x1000) ? 1 : 0;
   this.flags.P = ((hl & 0x8000) === (operand & 0x8000)) && ((result & 0x8000) !== (hl & 0x8000)) ? 1 : 0;
   this.flags.N = 0;
   this.flags.C = (result & 0x10000) ? 1 : 0;
   
   this.l = result & 0xff;
   this.h = (result >>> 8) & 0xff;
   
   this.update_xy_flags(this.h);
};

Z80.prototype.do_hl_sbc = function(operand)
{
   operand += this.flags.C;
   var hl = this.l | (this.h << 8), result = hl - operand;
   
   this.flags.S = (result & 0x8000) ? 1 : 0;
   this.flags.Z = !(result & 0xffff) ? 1 : 0;
   this.flags.H = (((hl & 0x0fff) - (operand & 0x0fff)) & 0x1000) ? 1 : 0;
   this.flags.P = ((hl & 0x8000) !== (operand & 0x8000)) && ((result & 0x8000) !== (hl & 0x8000)) ? 1 : 0;
   this.flags.N = 1;
   this.flags.C = (result & 0x10000) ? 1 : 0;
   
   this.l = result & 0xff;
   this.h = (result >>> 8) & 0xff;
   
   this.update_xy_flags(this.h);
};

Z80.prototype.do_in = function(port)
{
   var result = this.core.io_read(port);
   
   this.flags.S = (result & 0x80) ? 1 : 0;
   this.flags.Z = result ? 0 : 1;
   this.flags.H = 0;
   this.flags.P = this.get_parity(result) ? 1 : 0;
   this.flags.N = 0;
   this.update_xy_flags(result);
   
   return result;
};

Z80.prototype.do_neg = function()
{
   // This instruction is defined to not alter the register if it === 0x80.
   if (this.a !== 0x80)
   {
      // This is a signed operation, so convert A to a signed value.
      this.a = this.get_signed_offset_byte(this.a);
      
      this.a = (-this.a) & 0xff;
   }
   
   this.flags.S = (this.a & 0x80) ? 1 : 0;
   this.flags.Z = !this.a ? 1 : 0;
   this.flags.H = (((-this.a) & 0x0f) > 0) ? 1 : 0;
   this.flags.P = (this.a === 0x80) ? 1 : 0;
   this.flags.N = 1;
   this.flags.C = this.a ? 1 : 0;
   this.update_xy_flags(this.a);
};

Z80.prototype.do_ldi = function()
{
   // Copy the value that we're supposed to copy.
   var read_value = this.core.mem_read(this.l | (this.h << 8));
   this.core.mem_write(this.e | (this.d << 8), read_value);
   
   // Increment DE and HL, and decrement BC.
   var result = (this.e | (this.d << 8)) + 1;
   this.e = result & 0xff;
   this.d = (result & 0xff00) >>> 8;
   result = (this.l | (this.h << 8)) + 1;
   this.l = result & 0xff;
   this.h = (result & 0xff00) >>> 8;
   result = (this.c | (this.b << 8)) - 1;
   this.c = result & 0xff;
   this.b = (result & 0xff00) >>> 8;
   
   this.flags.H = 0;
   this.flags.P = (this.c || this.b) ? 1 : 0;
   this.flags.N = 0;
   this.flags.Y = ((this.a + read_value) & 0x02) >>> 1;
   this.flags.X = ((this.a + read_value) & 0x08) >>> 3;
};

Z80.prototype.do_cpi = function()
{
   var temp_carry = this.flags.C;
   var read_value = this.core.mem_read(this.l | (this.h << 8))
   this.do_cp(read_value);
   this.flags.C = temp_carry;
   this.flags.Y = ((this.a - read_value - this.flags.H) & 0x02) >>> 1;
   this.flags.X = ((this.a - read_value - this.flags.H) & 0x08) >>> 3;
   
   var result = (this.l | (this.h << 8)) + 1;
   this.l = result & 0xff;
   this.h = (result & 0xff00) >>> 8;
   result = (this.c | (this.b << 8)) - 1;
   this.c = result & 0xff;
   this.b = (result & 0xff00) >>> 8;
   
   this.flags.P = result ? 1 : 0;
};

Z80.prototype.do_ini = function()
{
   this.b = this.do_dec(this.b);
   
   this.core.mem_write(this.l | (this.h << 8), this.core.io_read((this.b << 8) | this.c));
   
   var result = (this.l | (this.h << 8)) + 1;
   this.l = result & 0xff;
   this.h = (result & 0xff00) >>> 8;

   this.flags.N = 1;
};

Z80.prototype.do_outi = function()
{
   this.core.io_write((this.b << 8) | this.c, this.core.mem_read(this.l | (this.h << 8)));
   
   var result = (this.l | (this.h << 8)) + 1;
   this.l = result & 0xff;
   this.h = (result & 0xff00) >>> 8;
   
   this.b = this.do_dec(this.b);
   this.flags.N = 1;
};

Z80.prototype.do_ldd = function()
{
   this.flags.N = 0;
   this.flags.H = 0;
   
   var read_value = this.core.mem_read(this.l | (this.h << 8));
   this.core.mem_write(this.e | (this.d << 8), read_value);
   
   var result = (this.e | (this.d << 8)) - 1;
   this.e = result & 0xff;
   this.d = (result & 0xff00) >>> 8;
   result = (this.l | (this.h << 8)) - 1;
   this.l = result & 0xff;
   this.h = (result & 0xff00) >>> 8;
   result = (this.c | (this.b << 8)) - 1;
   this.c = result & 0xff;
   this.b = (result & 0xff00) >>> 8;
   
   this.flags.P = (this.c || this.b) ? 1 : 0;
   this.flags.Y = ((this.a + read_value) & 0x02) >>> 1;
   this.flags.X = ((this.a + read_value) & 0x08) >>> 3;
};

Z80.prototype.do_cpd = function()
{
   var temp_carry = this.flags.C
   var read_value = this.core.mem_read(this.l | (this.h << 8))
   this.do_cp(read_value);
   this.flags.C = temp_carry;
   this.flags.Y = ((this.a - read_value - this.flags.H) & 0x02) >>> 1;
   this.flags.X = ((this.a - read_value - this.flags.H) & 0x08) >>> 3;
   
   var result = (this.l | (this.h << 8)) - 1;
   this.l = result & 0xff;
   this.h = (result & 0xff00) >>> 8;
   result = (this.c | (this.b << 8)) - 1;
   this.c = result & 0xff;
   this.b = (result & 0xff00) >>> 8;
   
   this.flags.P = result ? 1 : 0;
};

Z80.prototype.do_ind = function()
{
   this.b = this.do_dec(this.b);
   
   this.core.mem_write(this.l | (this.h << 8), this.core.io_read((this.b << 8) | this.c));
   
   var result = (this.l | (this.h << 8)) - 1;
   this.l = result & 0xff;
   this.h = (result & 0xff00) >>> 8;
   
   this.flags.N = 1;
};

Z80.prototype.do_outd = function()
{
   this.core.io_write((this.b << 8) | this.c, this.core.mem_read(this.l | (this.h << 8)));
   
   var result = (this.l | (this.h << 8)) - 1;
   this.l = result & 0xff;
   this.h = (result & 0xff00) >>> 8;
   
   this.b = this.do_dec(this.b);
   this.flags.N = 1;
};

Z80.prototype.do_rlc = function(operand)
{
   this.flags.N = 0;
   this.flags.H = 0;
   
   this.flags.C = (operand & 0x80) >>> 7;
   operand = ((operand << 1) | this.flags.C) & 0xff;
   
   this.flags.Z = !operand ? 1 : 0;
   this.flags.P = this.get_parity(operand);
   this.flags.S = (operand & 0x80) ? 1 : 0;
   this.update_xy_flags(operand);
   
   return operand;
};

Z80.prototype.do_rrc = function(operand)
{
   this.flags.N = 0;
   this.flags.H = 0;
   
   this.flags.C = operand & 1;
   operand = ((operand >>> 1) & 0x7f) | (this.flags.C << 7);
   
   this.flags.Z = !(operand & 0xff) ? 1 : 0;
   this.flags.P = this.get_parity(operand);
   this.flags.S = (operand & 0x80) ? 1 : 0;
   this.update_xy_flags(operand);
   
   return operand & 0xff;
};

Z80.prototype.do_rl = function(operand)
{
   this.flags.N = 0;
   this.flags.H = 0;
   
   var temp = this.flags.C;
   this.flags.C = (operand & 0x80) >>> 7;
   operand = ((operand << 1) | temp) & 0xff;
   
   this.flags.Z = !operand ? 1 : 0;
   this.flags.P = this.get_parity(operand);
   this.flags.S = (operand & 0x80) ? 1 : 0;
   this.update_xy_flags(operand);

   return operand;
};

Z80.prototype.do_rr = function(operand)
{
   this.flags.N = 0;
   this.flags.H = 0;
   
   var temp = this.flags.C;
   this.flags.C = operand & 1;
   operand = ((operand >>> 1) & 0x7f) | (temp << 7);
   
   this.flags.Z = !operand ? 1 : 0;
   this.flags.P = this.get_parity(operand);
   this.flags.S = (operand & 0x80) ? 1 : 0;
   this.update_xy_flags(operand);

   return operand;
};

Z80.prototype.do_sla = function(operand)
{
   this.flags.N = 0;
   this.flags.H = 0;
   
   this.flags.C = (operand & 0x80) >>> 7;
   operand = (operand << 1) & 0xff;
   
   this.flags.Z = !operand ? 1 : 0;
   this.flags.P = this.get_parity(operand);
   this.flags.S = (operand & 0x80) ? 1 : 0;
   this.update_xy_flags(operand);
   
   return operand;
};

Z80.prototype.do_sra = function(operand)
{
   this.flags.N = 0;
   this.flags.H = 0;
   
   this.flags.C = operand & 1;
   operand = ((operand >>> 1) & 0x7f) | (operand & 0x80);
   
   this.flags.Z = !operand ? 1 : 0;
   this.flags.P = this.get_parity(operand);
   this.flags.S = (operand & 0x80) ? 1 : 0;
   this.update_xy_flags(operand);
   
   return operand;
};

Z80.prototype.do_sll = function(operand)
{
   this.flags.N = 0;
   this.flags.H = 0;
   
   this.flags.C = (operand & 0x80) >>> 7;
   operand = ((operand << 1) & 0xff) | 1;
   
   this.flags.Z = !operand ? 1 : 0;
   this.flags.P = this.get_parity(operand);
   this.flags.S = (operand & 0x80) ? 1 : 0;
   this.update_xy_flags(operand);
   
   return operand;
};

Z80.prototype.do_srl = function(operand)
{
   this.flags.N = 0;
   this.flags.H = 0;
   
   this.flags.C = operand & 1;
   operand = (operand >>> 1) & 0x7f;
   
   this.flags.Z = !operand ? 1 : 0;
   this.flags.P = this.get_parity(operand);
   this.flags.S = 0;
   this.update_xy_flags(operand);
   
   return operand;
};

Z80.prototype.do_ix_add = function(operand)
{
   this.flags.N = 0;
   
   var result = this.ix + operand;
   
   this.flags.C = (result & 0x10000) ? 1 : 0;
   this.flags.H = (((this.ix & 0xfff) + (operand & 0xfff)) & 0x1000) ? 1 : 0;
   this.update_xy_flags((result & 0xff00) >>> 8);
   
   this.ix = result;
};


///////////////////////////////////////////////////////////////////////////////
/// This table contains the implementations for the instructions that weren't
///  implemented directly in the decoder function (everything but the 8-bit
///  register loads and the accumulator ALU instructions, in other words).
/// Similar tables for the ED and DD/FD prefixes follow this one.
///////////////////////////////////////////////////////////////////////////////
Z80.prototype.instructions = [];

// 0x00 : NOP
Z80.prototype.instructions[0x00] = function() { };
// 0x01 : LD BC, nn
Z80.prototype.instructions[0x01] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   this.c = this.core.mem_read(this.pc);
   this.pc = (this.pc + 1) & 0xffff;
   this.b = this.core.mem_read(this.pc);
};
// 0x02 : LD (BC), A
Z80.prototype.instructions[0x02] = function()
{
   this.core.mem_write(this.c | (this.b << 8), this.a);
};
// 0x03 : INC BC
Z80.prototype.instructions[0x03] = function()
{
   var result = (this.c | (this.b << 8));
   result += 1;
   this.c = result & 0xff;
   this.b = (result & 0xff00) >>> 8;
};
// 0x04 : INC B
Z80.prototype.instructions[0x04] = function()
{
   this.b = this.do_inc(this.b);
};
// 0x05 : DEC B
Z80.prototype.instructions[0x05] = function()
{
   this.b = this.do_dec(this.b);
};
// 0x06 : LD B, n
Z80.prototype.instructions[0x06] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   this.b = this.core.mem_read(this.pc);
};
// 0x07 : RLCA
Z80.prototype.instructions[0x07] = function()
{
   // This instruction is implemented as a special case of the
   //  more general Z80-specific RLC instruction.
   // Specifially, RLCA is a version of RLC A that affects fewer flags.
   // The same applies to RRCA, RLA, and RRA.
   var temp_s = this.flags.S, temp_z = this.flags.Z, temp_p = this.flags.P;
   this.a = this.do_rlc(this.a);
   this.flags.S = temp_s;
   this.flags.Z = temp_z;
   this.flags.P = temp_p;
};
// 0x08 : EX AF, AF'
Z80.prototype.instructions[0x08] = function()
{
   var temp = this.a;
   this.a = this.a_prime;
   this.a_prime = temp;
   
   temp = this.get_flags_register();
   this.set_flags_register(this.get_flags_prime());
   this.set_flags_prime(temp);
};
// 0x09 : ADD HL, BC
Z80.prototype.instructions[0x09] = function()
{
   this.do_hl_add(this.c | (this.b << 8));
};
// 0x0a : LD A, (BC)
Z80.prototype.instructions[0x0a] = function()
{
   this.a = this.core.mem_read(this.c | (this.b << 8));
};
// 0x0b : DEC BC
Z80.prototype.instructions[0x0b] = function()
{
   var result = (this.c | (this.b << 8));
   result -= 1;
   this.c = result & 0xff;
   this.b = (result & 0xff00) >>> 8;
};
// 0x0c : INC C
Z80.prototype.instructions[0x0c] = function()
{
   this.c = this.do_inc(this.c);
};
// 0x0d : DEC C
Z80.prototype.instructions[0x0d] = function()
{
   this.c = this.do_dec(this.c);
};
// 0x0e : LD C, n
Z80.prototype.instructions[0x0e] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   this.c = this.core.mem_read(this.pc);
};
// 0x0f : RRCA
Z80.prototype.instructions[0x0f] = function()
{
   var temp_s = this.flags.S, temp_z = this.flags.Z, temp_p = this.flags.P;
   this.a = this.do_rrc(this.a);
   this.flags.S = temp_s;
   this.flags.Z = temp_z;
   this.flags.P = temp_p;
};
// 0x10 : DJNZ nn
Z80.prototype.instructions[0x10] = function()
{
   this.b = (this.b - 1) & 0xff;
   this.do_conditional_relative_jump(this.b !== 0);
};
// 0x11 : LD DE, nn
Z80.prototype.instructions[0x11] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   this.e = this.core.mem_read(this.pc);
   this.pc = (this.pc + 1) & 0xffff;
   this.d = this.core.mem_read(this.pc);
};
// 0x12 : LD (DE), A
Z80.prototype.instructions[0x12] = function()
{
   this.core.mem_write(this.e | (this.d << 8), this.a);
};
// 0x13 : INC DE
Z80.prototype.instructions[0x13] = function()
{
   var result = (this.e | (this.d << 8));
   result += 1;
   this.e = result & 0xff;
   this.d = (result & 0xff00) >>> 8;
};
// 0x14 : INC D
Z80.prototype.instructions[0x14] = function()
{
   this.d = this.do_inc(this.d);
};
// 0x15 : DEC D
Z80.prototype.instructions[0x15] = function()
{
   this.d = this.do_dec(this.d);
};
// 0x16 : LD D, n
Z80.prototype.instructions[0x16] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   this.d = this.core.mem_read(this.pc);
};
// 0x17 : RLA
Z80.prototype.instructions[0x17] = function()
{
   var temp_s = this.flags.S, temp_z = this.flags.Z, temp_p = this.flags.P;
   this.a = this.do_rl(this.a);
   this.flags.S = temp_s;
   this.flags.Z = temp_z;
   this.flags.P = temp_p;
};
// 0x18 : JR n
Z80.prototype.instructions[0x18] = function()
{
   var offset = this.get_signed_offset_byte(this.core.mem_read((this.pc + 1) & 0xffff));
   this.pc = (this.pc + offset + 1) & 0xffff;
};
// 0x19 : ADD HL, DE
Z80.prototype.instructions[0x19] = function()
{
   this.do_hl_add(this.e | (this.d << 8));
};
// 0x1a : LD A, (DE)
Z80.prototype.instructions[0x1a] = function()
{
   this.a = this.core.mem_read(this.e | (this.d << 8));
};
// 0x1b : DEC DE
Z80.prototype.instructions[0x1b] = function()
{
   var result = (this.e | (this.d << 8));
   result -= 1;
   this.e = result & 0xff;
   this.d = (result & 0xff00) >>> 8;
};
// 0x1c : INC E
Z80.prototype.instructions[0x1c] = function()
{
   this.e = this.do_inc(this.e);
};
// 0x1d : DEC E
Z80.prototype.instructions[0x1d] = function()
{
   this.e = this.do_dec(this.e);
};
// 0x1e : LD E, n
Z80.prototype.instructions[0x1e] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   this.e = this.core.mem_read(this.pc);
};
// 0x1f : RRA
Z80.prototype.instructions[0x1f] = function()
{
   var temp_s = this.flags.S, temp_z = this.flags.Z, temp_p = this.flags.P;
   this.a = this.do_rr(this.a);
   this.flags.S = temp_s;
   this.flags.Z = temp_z;
   this.flags.P = temp_p;
};
// 0x20 : JR NZ, n
Z80.prototype.instructions[0x20] = function()
{
   this.do_conditional_relative_jump(!this.flags.Z);
};
// 0x21 : LD HL, nn
Z80.prototype.instructions[0x21] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   this.l = this.core.mem_read(this.pc);
   this.pc = (this.pc + 1) & 0xffff;
   this.h = this.core.mem_read(this.pc);
};
// 0x22 : LD (nn), HL
Z80.prototype.instructions[0x22] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   var address = this.core.mem_read(this.pc);
   this.pc = (this.pc + 1) & 0xffff;
   address |= this.core.mem_read(this.pc) << 8;
   
   this.core.mem_write(address, this.l);
   this.core.mem_write((address + 1) & 0xffff, this.h);
};
// 0x23 : INC HL
Z80.prototype.instructions[0x23] = function()
{
   var result = (this.l | (this.h << 8));
   result += 1;
   this.l = result & 0xff;
   this.h = (result & 0xff00) >>> 8;
};
// 0x24 : INC H
Z80.prototype.instructions[0x24] = function()
{
   this.h = this.do_inc(this.h);
};
// 0x25 : DEC H
Z80.prototype.instructions[0x25] = function()
{
   this.h = this.do_dec(this.h);
};
// 0x26 : LD H, n
Z80.prototype.instructions[0x26] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   this.h = this.core.mem_read(this.pc);
};
// 0x27 : DAA
Z80.prototype.instructions[0x27] = function()
{
   var temp = this.a;
   if (!this.flags.N)
   {
      if (this.flags.H || ((this.a & 0x0f) > 9))
         temp += 0x06;
      if (this.flags.C || (this.a > 0x99))
         temp += 0x60;
   }
   else
   {
      if (this.flags.H || ((this.a & 0x0f) > 9))
         temp -= 0x06;
      if (this.flags.C || (this.a > 0x99))
         temp -= 0x60;
   }
   
   this.flags.S = (temp & 0x80) ? 1 : 0;
   this.flags.Z = !(temp & 0xff) ? 1 : 0;
   this.flags.H = ((this.a & 0x10) ^ (temp & 0x10)) ? 1 : 0;
   this.flags.P = this.get_parity(temp & 0xff);
   // DAA never clears the carry flag if it was already set,
   //  but it is able to set the carry flag if it was clear.
   // Don't ask me, I don't know.
   // Note also that we check for a BCD carry, instead of the usual.
   this.flags.C = (this.flags.C || (this.a > 0x99)) ? 1 : 0;
   
   this.a = temp & 0xff;
   
   this.update_xy_flags(this.a);
};
// 0x28 : JR Z, n
Z80.prototype.instructions[0x28] = function()
{
   this.do_conditional_relative_jump(!!this.flags.Z);
};
// 0x29 : ADD HL, HL
Z80.prototype.instructions[0x29] = function()
{
   this.do_hl_add(this.l | (this.h << 8));
};
// 0x2a : LD HL, (nn)
Z80.prototype.instructions[0x2a] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   var address = this.core.mem_read(this.pc);
   this.pc = (this.pc + 1) & 0xffff;
   address |= this.core.mem_read(this.pc) << 8;
   
   this.l = this.core.mem_read(address);
   this.h = this.core.mem_read((address + 1) & 0xffff);
};
// 0x2b : DEC HL
Z80.prototype.instructions[0x2b] = function()
{
   var result = (this.l | (this.h << 8));
   result -= 1;
   this.l = result & 0xff;
   this.h = (result & 0xff00) >>> 8;
};
// 0x2c : INC L
Z80.prototype.instructions[0x2c] = function()
{
   this.l = this.do_inc(this.l);
};
// 0x2d : DEC L
Z80.prototype.instructions[0x2d] = function()
{
   this.l = this.do_dec(this.l);
};
// 0x2e : LD L, n
Z80.prototype.instructions[0x2e] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   this.l = this.core.mem_read(this.pc);
};
// 0x2f : CPL
Z80.prototype.instructions[0x2f] = function()
{
   this.a = (~this.a) & 0xff;
   this.flags.N = 1;
   this.flags.H = 1;
   this.update_xy_flags(this.a);
};
// 0x30 : JR NC, n
Z80.prototype.instructions[0x30] = function()
{
   this.do_conditional_relative_jump(!this.flags.C);
};
// 0x31 : LD SP, nn
Z80.prototype.instructions[0x31] = function()
{
   this.sp =  this.core.mem_read((this.pc + 1) & 0xffff) | 
            (this.core.mem_read((this.pc + 2) & 0xffff) << 8);
   this.pc = (this.pc + 2) & 0xffff;
};
// 0x32 : LD (nn), A
Z80.prototype.instructions[0x32] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   var address = this.core.mem_read(this.pc);
   this.pc = (this.pc + 1) & 0xffff;
   address |= this.core.mem_read(this.pc) << 8;
   
   this.core.mem_write(address, this.a);
};
// 0x33 : INC SP
Z80.prototype.instructions[0x33] = function()
{
   this.sp = (this.sp + 1) & 0xffff;
};
// 0x34 : INC (HL)
Z80.prototype.instructions[0x34] = function()
{
   var address = this.l | (this.h << 8);
   this.core.mem_write(address, this.do_inc(this.core.mem_read(address)));
};
// 0x35 : DEC (HL)
Z80.prototype.instructions[0x35] = function()
{
   var address = this.l | (this.h << 8);
   this.core.mem_write(address, this.do_dec(this.core.mem_read(address)));
};
// 0x36 : LD (HL), n
Z80.prototype.instructions[0x36] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   this.core.mem_write(this.l | (this.h << 8), this.core.mem_read(this.pc));
};
// 0x37 : SCF
Z80.prototype.instructions[0x37] = function()
{
   this.flags.N = 0;
   this.flags.H = 0;
   this.flags.C = 1;
   this.update_xy_flags(this.a);
};
// 0x38 : JR C, n
Z80.prototype.instructions[0x38] = function()
{
   this.do_conditional_relative_jump(!!this.flags.C);
};
// 0x39 : ADD HL, SP
Z80.prototype.instructions[0x39] = function()
{
   this.do_hl_add(this.sp);
};
// 0x3a : LD A, (nn)
Z80.prototype.instructions[0x3a] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   var address = this.core.mem_read(this.pc);
   this.pc = (this.pc + 1) & 0xffff;
   address |= this.core.mem_read(this.pc) << 8;
   
   this.a = this.core.mem_read(address);
};
// 0x3b : DEC SP
Z80.prototype.instructions[0x3b] = function()
{
   this.sp = (this.sp - 1) & 0xffff;
};
// 0x3c : INC A
Z80.prototype.instructions[0x3c] = function()
{
   this.a = this.do_inc(this.a);
};
// 0x3d : DEC A
Z80.prototype.instructions[0x3d] = function()
{
   this.a = this.do_dec(this.a);
};
// 0x3e : LD A, n
Z80.prototype.instructions[0x3e] = function()
{
   this.a = this.core.mem_read((this.pc + 1) & 0xffff);
   this.pc = (this.pc + 1) & 0xffff;
};
// 0x3f : CCF
Z80.prototype.instructions[0x3f] = function()
{
   this.flags.N = 0;
   this.flags.H = this.flags.C;
   this.flags.C = this.flags.C ? 0 : 1;
   this.update_xy_flags(this.a);
};
// 0xc0 : RET NZ
Z80.prototype.instructions[0xc0] = function()
{
   this.do_conditional_return(!this.flags.Z);
};
// 0xc1 : POP BC
Z80.prototype.instructions[0xc1] = function()
{
   var result = this.pop_word();
   this.c = result & 0xff;
   this.b = (result & 0xff00) >>> 8;
};
// 0xc2 : JP NZ, nn
Z80.prototype.instructions[0xc2] = function()
{
   this.do_conditional_absolute_jump(!this.flags.Z);
};
// 0xc3 : JP nn
Z80.prototype.instructions[0xc3] = function()
{
   this.pc =  this.core.mem_read((this.pc + 1) & 0xffff) |
            (this.core.mem_read((this.pc + 2) & 0xffff) << 8);
   this.pc = (this.pc - 1) & 0xffff;
};
// 0xc4 : CALL NZ, nn
Z80.prototype.instructions[0xc4] = function()
{
   this.do_conditional_call(!this.flags.Z);
};
// 0xc5 : PUSH BC
Z80.prototype.instructions[0xc5] = function()
{
   this.push_word(this.c | (this.b << 8));
};
// 0xc6 : ADD A, n
Z80.prototype.instructions[0xc6] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   this.do_add(this.core.mem_read(this.pc));
};
// 0xc7 : RST 00h
Z80.prototype.instructions[0xc7] = function()
{
   this.do_reset(0x00);
};
// 0xc8 : RET Z
Z80.prototype.instructions[0xc8] = function()
{
   this.do_conditional_return(!!this.flags.Z);
};
// 0xc9 : RET
Z80.prototype.instructions[0xc9] = function()
{
   this.pc = (this.pop_word() - 1) & 0xffff;
};
// 0xca : JP Z, nn
Z80.prototype.instructions[0xca] = function()
{
   this.do_conditional_absolute_jump(!!this.flags.Z);
};
// 0xcb : CB Prefix
Z80.prototype.instructions[0xcb] = function()
{
   // R is incremented at the start of the second instruction cycle,
   //  before the instruction actually runs.
   // The high bit of R is not affected by this increment,
   //  it can only be changed using the LD R, A instruction.
   this.r = (this.r & 0x80) | (((this.r & 0x7f) + 1) & 0x7f);

   // We don't have a table for this prefix,
   //  the instructions are all so uniform that we can directly decode them.
   this.pc = (this.pc + 1) & 0xffff;
   var opcode = this.core.mem_read(this.pc),
       bit_number = (opcode & 0x38) >>> 3,
       reg_code = opcode & 0x07;
   
   if (opcode < 0x40)
   {
      // Shift/rotate instructions
      var op_array = [this.do_rlc, this.do_rrc, this.do_rl, this.do_rr,
                      this.do_sla, this.do_sra, this.do_sll, this.do_srl];
      
      if (reg_code === 0)
         this.b = op_array[bit_number].call(this, this.b);
      else if (reg_code === 1)
         this.c = op_array[bit_number].call(this, this.c);
      else if (reg_code === 2)
         this.d = op_array[bit_number].call(this, this.d);
      else if (reg_code === 3)
         this.e = op_array[bit_number].call(this, this.e);
      else if (reg_code === 4)
         this.h = op_array[bit_number].call(this, this.h);
      else if (reg_code === 5)
         this.l = op_array[bit_number].call(this, this.l);
      else if (reg_code === 6)
         this.core.mem_write(this.l | (this.h << 8),
                            op_array[bit_number].call(this, this.core.mem_read(this.l | (this.h << 8))));
      else if (reg_code === 7)
         this.a = op_array[bit_number].call(this, this.a);
   }
   else if (opcode < 0x80)
   {
      // BIT instructions
      if (reg_code === 0)
         this.flags.Z = !(this.b & (1 << bit_number)) ? 1 : 0;
      else if (reg_code === 1)
         this.flags.Z = !(this.c & (1 << bit_number)) ? 1 : 0;
      else if (reg_code === 2)
         this.flags.Z = !(this.d & (1 << bit_number)) ? 1 : 0;
      else if (reg_code === 3)
         this.flags.Z = !(this.e & (1 << bit_number)) ? 1 : 0;
      else if (reg_code === 4)
         this.flags.Z = !(this.h & (1 << bit_number)) ? 1 : 0;
      else if (reg_code === 5)
         this.flags.Z = !(this.l & (1 << bit_number)) ? 1 : 0;
      else if (reg_code === 6)
         this.flags.Z = !((this.core.mem_read(this.l | (this.h << 8))) & (1 << bit_number)) ? 1 : 0;
      else if (reg_code === 7)
         this.flags.Z = !(this.a & (1 << bit_number)) ? 1 : 0;
         
      this.flags.N = 0;
      this.flags.H = 1;
      this.flags.P = this.flags.Z;
      this.flags.S = ((bit_number === 7) && !this.flags.Z) ? 1 : 0;
      // For the BIT n, (HL) instruction, the X and Y flags are obtained
      //  from what is apparently an internal temporary register used for
      //  some of the 16-bit arithmetic instructions.
      // I haven't implemented that register here,
      //  so for now we'll set X and Y the same way for every BIT opcode,
      //  which means that they will usually be wrong for BIT n, (HL).
      this.flags.Y = ((bit_number === 5) && !this.flags.Z) ? 1 : 0;
      this.flags.X = ((bit_number === 3) && !this.flags.Z) ? 1 : 0;
   }
   else if (opcode < 0xc0)
   {
      // RES instructions
      if (reg_code === 0)
         this.b &= (0xff & ~(1 << bit_number));
      else if (reg_code === 1)
         this.c &= (0xff & ~(1 << bit_number));
      else if (reg_code === 2)
         this.d &= (0xff & ~(1 << bit_number));
      else if (reg_code === 3)
         this.e &= (0xff & ~(1 << bit_number));
      else if (reg_code === 4)
         this.h &= (0xff & ~(1 << bit_number));
      else if (reg_code === 5)
         this.l &= (0xff & ~(1 << bit_number));
      else if (reg_code === 6)
         this.core.mem_write(this.l | (this.h << 8),
                            this.core.mem_read(this.l | (this.h << 8)) & ~(1 << bit_number));
      else if (reg_code === 7)
         this.a &= (0xff & ~(1 << bit_number));
   }
   else
   {
      // SET instructions
      if (reg_code === 0)
         this.b |= (1 << bit_number);
      else if (reg_code === 1)
         this.c |= (1 << bit_number);
      else if (reg_code === 2)
         this.d |= (1 << bit_number);
      else if (reg_code === 3)
         this.e |= (1 << bit_number);
      else if (reg_code === 4)
         this.h |= (1 << bit_number);
      else if (reg_code === 5)
         this.l |= (1 << bit_number);
      else if (reg_code === 6)
         this.core.mem_write(this.l | (this.h << 8),
                            this.core.mem_read(this.l | (this.h << 8)) | (1 << bit_number));
      else if (reg_code === 7)
         this.a |= (1 << bit_number);
   }
   
   this.cycle_counter += this.cycle_counts_cb[opcode];
};
// 0xcc : CALL Z, nn
Z80.prototype.instructions[0xcc] = function()
{
   this.do_conditional_call(!!this.flags.Z);
};
// 0xcd : CALL nn
Z80.prototype.instructions[0xcd] = function()
{
   this.push_word((this.pc + 3) & 0xffff);
   this.pc =  this.core.mem_read((this.pc + 1) & 0xffff) |
            (this.core.mem_read((this.pc + 2) & 0xffff) << 8);
   this.pc = (this.pc - 1) & 0xffff;
};
// 0xce : ADC A, n
Z80.prototype.instructions[0xce] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   this.do_adc(this.core.mem_read(this.pc));
};
// 0xcf : RST 08h
Z80.prototype.instructions[0xcf] = function()
{
   this.do_reset(0x08);
};
// 0xd0 : RET NC
Z80.prototype.instructions[0xd0] = function()
{
   this.do_conditional_return(!this.flags.C);
};
// 0xd1 : POP DE
Z80.prototype.instructions[0xd1] = function()
{
   var result = this.pop_word();
   this.e = result & 0xff;
   this.d = (result & 0xff00) >>> 8;
};
// 0xd2 : JP NC, nn
Z80.prototype.instructions[0xd2] = function()
{
   this.do_conditional_absolute_jump(!this.flags.C);
};
// 0xd3 : OUT (n), A
Z80.prototype.instructions[0xd3] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   this.core.io_write((this.a << 8) | this.core.mem_read(this.pc), this.a);
};
// 0xd4 : CALL NC, nn
Z80.prototype.instructions[0xd4] = function()
{
   this.do_conditional_call(!this.flags.C);
};
// 0xd5 : PUSH DE
Z80.prototype.instructions[0xd5] = function()
{
   this.push_word(this.e | (this.d << 8));
};
// 0xd6 : SUB n
Z80.prototype.instructions[0xd6] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   this.do_sub(this.core.mem_read(this.pc));
};
// 0xd7 : RST 10h
Z80.prototype.instructions[0xd7] = function()
{
   this.do_reset(0x10);
};
// 0xd8 : RET C
Z80.prototype.instructions[0xd8] = function()
{
   this.do_conditional_return(!!this.flags.C);
};
// 0xd9 : EXX
Z80.prototype.instructions[0xd9] = function()
{
   var temp = this.b;
   this.b = this.b_prime;
   this.b_prime = temp;
   temp = this.c;
   this.c = this.c_prime;
   this.c_prime = temp;
   temp = this.d;
   this.d = this.d_prime;
   this.d_prime = temp;
   temp = this.e;
   this.e = this.e_prime;
   this.e_prime = temp;
   temp = this.h;
   this.h = this.h_prime;
   this.h_prime = temp;
   temp = this.l;
   this.l = this.l_prime;
   this.l_prime = temp;
};
// 0xda : JP C, nn
Z80.prototype.instructions[0xda] = function()
{
   this.do_conditional_absolute_jump(!!this.flags.C);
};
// 0xdb : IN A, (n)
Z80.prototype.instructions[0xdb] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   this.a = this.core.io_read((this.a << 8) | this.core.mem_read(this.pc));
};
// 0xdc : CALL C, nn
Z80.prototype.instructions[0xdc] = function()
{
   this.do_conditional_call(!!this.flags.C);
};
// 0xdd : DD Prefix (IX instructions)
Z80.prototype.instructions[0xdd] = function()
{
   // R is incremented at the start of the second instruction cycle,
   //  before the instruction actually runs.
   // The high bit of R is not affected by this increment,
   //  it can only be changed using the LD R, A instruction.
   this.r = (this.r & 0x80) | (((this.r & 0x7f) + 1) & 0x7f);

   this.pc = (this.pc + 1) & 0xffff;
   var opcode = this.core.mem_read(this.pc),
       func = this.dd_instructions[opcode];
       
   if (func)
   {
      func = func.bind(this);
      func();
      this.cycle_counter += this.cycle_counts_dd[opcode];
   }
   else
   {
      // Apparently if a DD opcode doesn't exist,
      //  it gets treated as an unprefixed opcode.
      // What we'll do to handle that is just back up the 
      //  program counter, so that this byte gets decoded
      //  as a normal instruction.
      this.pc = (this.pc - 1) & 0xffff;
      // And we'll add in the cycle count for a NOP.
      this.cycle_counter += this.cycle_counts[0];
   }
};
// 0xde : SBC n
Z80.prototype.instructions[0xde] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   this.do_sbc(this.core.mem_read(this.pc));
};
// 0xdf : RST 18h
Z80.prototype.instructions[0xdf] = function()
{
   this.do_reset(0x18);
};
// 0xe0 : RET PO
Z80.prototype.instructions[0xe0] = function()
{
   this.do_conditional_return(!this.flags.P);
};
// 0xe1 : POP HL
Z80.prototype.instructions[0xe1] = function()
{
   var result = this.pop_word();
   this.l = result & 0xff;
   this.h = (result & 0xff00) >>> 8;
};
// 0xe2 : JP PO, (nn)
Z80.prototype.instructions[0xe2] = function()
{
   this.do_conditional_absolute_jump(!this.flags.P);
};
// 0xe3 : EX (SP), HL
Z80.prototype.instructions[0xe3] = function()
{
   var temp = this.core.mem_read(this.sp);
   this.core.mem_write(this.sp, this.l);
   this.l = temp;
   temp = this.core.mem_read((this.sp + 1) & 0xffff);
   this.core.mem_write((this.sp + 1) & 0xffff, this.h);
   this.h = temp;
};
// 0xe4 : CALL PO, nn
Z80.prototype.instructions[0xe4] = function()
{
   this.do_conditional_call(!this.flags.P);
};
// 0xe5 : PUSH HL
Z80.prototype.instructions[0xe5] = function()
{
   this.push_word(this.l | (this.h << 8));
};
// 0xe6 : AND n
Z80.prototype.instructions[0xe6] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   this.do_and(this.core.mem_read(this.pc));
};
// 0xe7 : RST 20h
Z80.prototype.instructions[0xe7] = function()
{
   this.do_reset(0x20);
};
// 0xe8 : RET PE
Z80.prototype.instructions[0xe8] = function()
{
   this.do_conditional_return(!!this.flags.P);
};
// 0xe9 : JP (HL)
Z80.prototype.instructions[0xe9] = function()
{
   this.pc = this.l | (this.h << 8);
   this.pc = (this.pc - 1) & 0xffff;
};
// 0xea : JP PE, nn
Z80.prototype.instructions[0xea] = function()
{
   this.do_conditional_absolute_jump(!!this.flags.P);
};
// 0xeb : EX DE, HL
Z80.prototype.instructions[0xeb] = function()
{
   var temp = this.d;
   this.d = this.h;
   this.h = temp;
   temp = this.e;
   this.e = this.l;
   this.l = temp;
};
// 0xec : CALL PE, nn
Z80.prototype.instructions[0xec] = function()
{
   this.do_conditional_call(!!this.flags.P);
};
// 0xed : ED Prefix
Z80.prototype.instructions[0xed] = function()
{
   // R is incremented at the start of the second instruction cycle,
   //  before the instruction actually runs.
   // The high bit of R is not affected by this increment,
   //  it can only be changed using the LD R, A instruction.
   this.r = (this.r & 0x80) | (((this.r & 0x7f) + 1) & 0x7f);

   this.pc = (this.pc + 1) & 0xffff;
   var opcode = this.core.mem_read(this.pc),
       func = this.ed_instructions[opcode];
       
   if (func)
   {
      func = func.bind(this);
      func();
      this.cycle_counter += this.cycle_counts_ed[opcode];
   }
   else
   {
      // If the opcode didn't exist, the whole thing is a two-byte NOP.
      this.cycle_counter += this.cycle_counts[0];
   }
};
// 0xee : XOR n
Z80.prototype.instructions[0xee] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   this.do_xor(this.core.mem_read(this.pc));
};
// 0xef : RST 28h
Z80.prototype.instructions[0xef] = function()
{
   this.do_reset(0x28);
};
// 0xf0 : RET P
Z80.prototype.instructions[0xf0] = function()
{
   this.do_conditional_return(!this.flags.S);
};
// 0xf1 : POP AF
Z80.prototype.instructions[0xf1] = function()
{
   var result = this.pop_word();
   this.set_flags_register(result & 0xff);
   this.a = (result & 0xff00) >>> 8;
};
// 0xf2 : JP P, nn
Z80.prototype.instructions[0xf2] = function()
{
   this.do_conditional_absolute_jump(!this.flags.S);
};
// 0xf3 : DI
Z80.prototype.instructions[0xf3] = function()
{
   // DI doesn't actually take effect until after the next instruction.
   this.do_delayed_di = true;
};
// 0xf4 : CALL P, nn
Z80.prototype.instructions[0xf4] = function()
{
   this.do_conditional_call(!this.flags.S);
};
// 0xf5 : PUSH AF
Z80.prototype.instructions[0xf5] = function()
{
   this.push_word(this.get_flags_register() | (this.a << 8));
};
// 0xf6 : OR n
Z80.prototype.instructions[0xf6] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   this.do_or(this.core.mem_read(this.pc));
};
// 0xf7 : RST 30h
Z80.prototype.instructions[0xf7] = function()
{
   this.do_reset(0x30);
};
// 0xf8 : RET M
Z80.prototype.instructions[0xf8] = function()
{
   this.do_conditional_return(!!this.flags.S);
};
// 0xf9 : LD SP, HL
Z80.prototype.instructions[0xf9] = function()
{
   this.sp = this.l | (this.h << 8);
};
// 0xfa : JP M, nn
Z80.prototype.instructions[0xfa] = function()
{
   this.do_conditional_absolute_jump(!!this.flags.S);
};
// 0xfb : EI
Z80.prototype.instructions[0xfb] = function()
{
   // EI doesn't actually take effect until after the next instruction.
   this.do_delayed_ei = true;
};
// 0xfc : CALL M, nn
Z80.prototype.instructions[0xfc] = function()
{
   this.do_conditional_call(!!this.flags.S);
};
// 0xfd : FD Prefix (IY instructions)
Z80.prototype.instructions[0xfd] = function()
{
   // R is incremented at the start of the second instruction cycle,
   //  before the instruction actually runs.
   // The high bit of R is not affected by this increment,
   //  it can only be changed using the LD R, A instruction.
   this.r = (this.r & 0x80) | (((this.r & 0x7f) + 1) & 0x7f);
   
   this.pc = (this.pc + 1) & 0xffff;
   var opcode = this.core.mem_read(this.pc),
       func = this.dd_instructions[opcode];
       
   if (func)
   {
      // Rather than copy and paste all the IX instructions into IY instructions,
      //  what we'll do is sneakily copy IY into IX, run the IX instruction,
      //  and then copy the result into IY and restore the old IX.
      var temp = this.ix;
      this.ix = this.iy;
      func = func.bind(this);
      func();
      this.iy = this.ix;
      this.ix = temp;
      
      this.cycle_counter += this.cycle_counts_dd[opcode];
   }
   else
   {
      // Apparently if an FD opcode doesn't exist,
      //  it gets treated as an unprefixed opcode.
      // What we'll do to handle that is just back up the 
      //  program counter, so that this byte gets decoded
      //  as a normal instruction.
      this.pc = (this.pc - 1) & 0xffff;
      // And we'll add in the cycle count for a NOP.
      this.cycle_counter += this.cycle_counts[0];
   }
};
// 0xfe : CP n
Z80.prototype.instructions[0xfe] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   this.do_cp(this.core.mem_read(this.pc));
};
// 0xff : RST 38h
Z80.prototype.instructions[0xff] = function()
{
   this.do_reset(0x38);
};


///////////////////////////////////////////////////////////////////////////////
/// This table of ED opcodes is pretty sparse;
///  there are not very many valid ED-prefixed opcodes in the Z80,
///  and many of the ones that are valid are not documented.
///////////////////////////////////////////////////////////////////////////////
Z80.prototype.ed_instructions = [];
// 0x40 : IN B, (C)
Z80.prototype.ed_instructions[0x40] = function()
{
   this.b = this.do_in((this.b << 8) | this.c);
};
// 0x41 : OUT (C), B
Z80.prototype.ed_instructions[0x41] = function()
{
   this.core.io_write((this.b << 8) | this.c, this.b);
};
// 0x42 : SBC HL, BC
Z80.prototype.ed_instructions[0x42] = function()
{
   this.do_hl_sbc(this.c | (this.b << 8));
};
// 0x43 : LD (nn), BC
Z80.prototype.ed_instructions[0x43] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   var address = this.core.mem_read(this.pc);
   this.pc = (this.pc + 1) & 0xffff;
   address |= this.core.mem_read(this.pc) << 8;
   
   this.core.mem_write(address, this.c);
   this.core.mem_write((address + 1) & 0xffff, this.b);
};
// 0x44 : NEG
Z80.prototype.ed_instructions[0x44] = function()
{
   this.do_neg();
};
// 0x45 : RETN
Z80.prototype.ed_instructions[0x45] = function()
{
   this.pc = (this.pop_word() - 1) & 0xffff;
   this.iff1 = this.iff2;
};
// 0x46 : IM 0
Z80.prototype.ed_instructions[0x46] = function()
{
   this.imode = 0;
};
// 0x47 : LD I, A
Z80.prototype.ed_instructions[0x47] = function()
{
   this.i = this.a
};
// 0x48 : IN C, (C)
Z80.prototype.ed_instructions[0x48] = function()
{
   this.c = this.do_in((this.b << 8) | this.c);
};
// 0x49 : OUT (C), C
Z80.prototype.ed_instructions[0x49] = function()
{
   this.core.io_write((this.b << 8) | this.c, this.c);
};
// 0x4a : ADC HL, BC
Z80.prototype.ed_instructions[0x4a] = function()
{
   this.do_hl_adc(this.c | (this.b << 8));
};
// 0x4b : LD BC, (nn)
Z80.prototype.ed_instructions[0x4b] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   var address = this.core.mem_read(this.pc);
   this.pc = (this.pc + 1) & 0xffff;
   address |= this.core.mem_read(this.pc) << 8;
   
   this.c = this.core.mem_read(address);
   this.b = this.core.mem_read((address + 1) & 0xffff);
};
// 0x4c : NEG (Undocumented)
Z80.prototype.ed_instructions[0x4c] = function()
{
   this.do_neg();
};
// 0x4d : RETI
Z80.prototype.ed_instructions[0x4d] = function()
{
   this.pc = (this.pop_word() - 1) & 0xffff;
};
// 0x4e : IM 0 (Undocumented)
Z80.prototype.ed_instructions[0x4e] = function()
{
   this.imode = 0;
};
// 0x4f : LD R, A
Z80.prototype.ed_instructions[0x4f] = function()
{
   this.r = this.a;
};
// 0x50 : IN D, (C)
Z80.prototype.ed_instructions[0x50] = function()
{
   this.d = this.do_in((this.b << 8) | this.c);
};
// 0x51 : OUT (C), D
Z80.prototype.ed_instructions[0x51] = function()
{
   this.core.io_write((this.b << 8) | this.c, this.d);
};
// 0x52 : SBC HL, DE
Z80.prototype.ed_instructions[0x52] = function()
{
   this.do_hl_sbc(this.e | (this.d << 8));
};
// 0x53 : LD (nn), DE
Z80.prototype.ed_instructions[0x53] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   var address = this.core.mem_read(this.pc);
   this.pc = (this.pc + 1) & 0xffff;
   address |= this.core.mem_read(this.pc) << 8;
   
   this.core.mem_write(address, this.e);
   this.core.mem_write((address + 1) & 0xffff, this.d);
};
// 0x54 : NEG (Undocumented)
Z80.prototype.ed_instructions[0x54] = function()
{
   this.do_neg();
};
// 0x55 : RETN
Z80.prototype.ed_instructions[0x55] = function()
{
   this.pc = (this.pop_word() - 1) & 0xffff;
   this.iff1 = this.iff2;
};
// 0x56 : IM 1
Z80.prototype.ed_instructions[0x56] = function()
{
   this.imode = 1;
};
// 0x57 : LD A, I
Z80.prototype.ed_instructions[0x57] = function()
{
   this.a = this.i;
   this.flags.P = this.iff2;
};
// 0x58 : IN E, (C)
Z80.prototype.ed_instructions[0x58] = function()
{
   this.e = this.do_in((this.b << 8) | this.c);
};
// 0x59 : OUT (C), E
Z80.prototype.ed_instructions[0x59] = function()
{
   this.core.io_write((this.b << 8) | this.c, this.e);
};
// 0x5a : ADC HL, DE
Z80.prototype.ed_instructions[0x5a] = function()
{
   this.do_hl_adc(this.e | (this.d << 8));
};
// 0x5b : LD DE, (nn)
Z80.prototype.ed_instructions[0x5b] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   var address = this.core.mem_read(this.pc);
   this.pc = (this.pc + 1) & 0xffff;
   address |= this.core.mem_read(this.pc) << 8;
   
   this.e = this.core.mem_read(address);
   this.d = this.core.mem_read((address + 1) & 0xffff);
};
// 0x5c : NEG (Undocumented)
Z80.prototype.ed_instructions[0x5c] = function()
{
   this.do_neg();
};
// 0x5d : RETN
Z80.prototype.ed_instructions[0x5d] = function()
{
   this.pc = (this.pop_word() - 1) & 0xffff;
   this.iff1 = this.iff2;
};
// 0x5e : IM 2
Z80.prototype.ed_instructions[0x5e] = function()
{
   this.imode = 2;
};
// 0x5f : LD A, R
Z80.prototype.ed_instructions[0x5f] = function()
{
   this.a = this.r;
   this.flags.P = this.iff2;
};
// 0x60 : IN H, (C)
Z80.prototype.ed_instructions[0x60] = function()
{
   this.h = this.do_in((this.b << 8) | this.c);
};
// 0x61 : OUT (C), H
Z80.prototype.ed_instructions[0x61] = function()
{
   this.core.io_write((this.b << 8) | this.c, this.h);
};
// 0x62 : SBC HL, HL
Z80.prototype.ed_instructions[0x62] = function()
{
   this.do_hl_sbc(this.l | (this.h << 8));
};
// 0x63 : LD (nn), HL (Undocumented)
Z80.prototype.ed_instructions[0x63] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   var address = this.core.mem_read(this.pc);
   this.pc = (this.pc + 1) & 0xffff;
   address |= this.core.mem_read(this.pc) << 8;
   
   this.core.mem_write(address, this.l);
   this.core.mem_write((address + 1) & 0xffff, this.h);
};
// 0x64 : NEG (Undocumented)
Z80.prototype.ed_instructions[0x64] = function()
{
   this.do_neg();
};
// 0x65 : RETN
Z80.prototype.ed_instructions[0x65] = function()
{
   this.pc = (this.pop_word() - 1) & 0xffff;
   this.iff1 = this.iff2;
};
// 0x66 : IM 0
Z80.prototype.ed_instructions[0x66] = function()
{
   this.imode = 0;
};
// 0x67 : RRD
Z80.prototype.ed_instructions[0x67] = function()
{
   var hl_value = this.core.mem_read(this.l | (this.h << 8));
   var temp1 = hl_value & 0x0f, temp2 = this.a & 0x0f;
   hl_value = ((hl_value & 0xf0) >>> 4) | (temp2 << 4);
   this.a = (this.a & 0xf0) | temp1;
   this.core.mem_write(this.l | (this.h << 8), hl_value);
   
   this.flags.S = (this.a & 0x80) ? 1 : 0;
   this.flags.Z = this.a ? 0 : 1;
   this.flags.H = 0;
   this.flags.P = this.get_parity(this.a) ? 1 : 0;
   this.flags.N = 0;
   this.update_xy_flags(this.a);
};
// 0x68 : IN L, (C)
Z80.prototype.ed_instructions[0x68] = function()
{
   this.l = this.do_in((this.b << 8) | this.c);
};
// 0x69 : OUT (C), L
Z80.prototype.ed_instructions[0x69] = function()
{
   this.core.io_write((this.b << 8) | this.c, this.l);
};
// 0x6a : ADC HL, HL
Z80.prototype.ed_instructions[0x6a] = function()
{
   this.do_hl_adc(this.l | (this.h << 8));
};
// 0x6b : LD HL, (nn) (Undocumented)
Z80.prototype.ed_instructions[0x6b] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   var address = this.core.mem_read(this.pc);
   this.pc = (this.pc + 1) & 0xffff;
   address |= this.core.mem_read(this.pc) << 8;
   
   this.l = this.core.mem_read(address);
   this.h = this.core.mem_read((address + 1) & 0xffff);
};
// 0x6c : NEG (Undocumented)
Z80.prototype.ed_instructions[0x6c] = function()
{
   this.do_neg();
};
// 0x6d : RETN
Z80.prototype.ed_instructions[0x6d] = function()
{
   this.pc = (this.pop_word() - 1) & 0xffff;
   this.iff1 = this.iff2;
};
// 0x6e : IM 0 (Undocumented)
Z80.prototype.ed_instructions[0x6e] = function()
{
   this.imode = 0;
};
// 0x6f : RLD
Z80.prototype.ed_instructions[0x6f] = function()
{
   var hl_value = this.core.mem_read(this.l | (this.h << 8));
   var temp1 = hl_value & 0xf0, temp2 = this.a & 0x0f;
   hl_value = ((hl_value & 0x0f) << 4) | temp2;
   this.a = (this.a & 0xf0) | (temp1 >>> 4);
   this.core.mem_write(this.l | (this.h << 8), hl_value);
   
   this.flags.S = (this.a & 0x80) ? 1 : 0;
   this.flags.Z = this.a ? 0 : 1;
   this.flags.H = 0;
   this.flags.P = this.get_parity(this.a) ? 1 : 0;
   this.flags.N = 0;
   this.update_xy_flags(this.a);
};
// 0x70 : IN (C) (Undocumented)
Z80.prototype.ed_instructions[0x70] = function()
{
   this.do_in((this.b << 8) | this.c);
};
// 0x71 : OUT (C), 0 (Undocumented)
Z80.prototype.ed_instructions[0x71] = function()
{
   this.core.io_write((this.b << 8) | this.c, 0);
};
// 0x72 : SBC HL, SP
Z80.prototype.ed_instructions[0x72] = function()
{
   this.do_hl_sbc(this.sp);
};
// 0x73 : LD (nn), SP
Z80.prototype.ed_instructions[0x73] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   var address = this.core.mem_read(this.pc);
   this.pc = (this.pc + 1) & 0xffff;
   address |= this.core.mem_read(this.pc) << 8;
   
   this.core.mem_write(address, this.sp & 0xff);
   this.core.mem_write((address + 1) & 0xffff, (this.sp >>> 8) & 0xff);
};
// 0x74 : NEG (Undocumented)
Z80.prototype.ed_instructions[0x74] = function()
{
   this.do_neg();
};
// 0x75 : RETN
Z80.prototype.ed_instructions[0x75] = function()
{
   this.pc = (this.pop_word() - 1) & 0xffff;
   this.iff1 = this.iff2;
};
// 0x76 : IM 1
Z80.prototype.ed_instructions[0x76] = function()
{
   this.imode = 1;
};
// 0x78 : IN A, (C)
Z80.prototype.ed_instructions[0x78] = function()
{
   this.a = this.do_in((this.b << 8) | this.c);
};
// 0x79 : OUT (C), A
Z80.prototype.ed_instructions[0x79] = function()
{
   this.core.io_write((this.b << 8) | this.c, this.a);
};
// 0x7a : ADC HL, SP
Z80.prototype.ed_instructions[0x7a] = function()
{
   this.do_hl_adc(this.sp);
};
// 0x7b : LD SP, (nn)
Z80.prototype.ed_instructions[0x7b] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   var address = this.core.mem_read(this.pc);
   this.pc = (this.pc + 1) & 0xffff;
   address |= this.core.mem_read(this.pc) << 8;
   
   this.sp = this.core.mem_read(address);
   this.sp |= this.core.mem_read((address + 1) & 0xffff) << 8;
};
// 0x7c : NEG (Undocumented)
Z80.prototype.ed_instructions[0x7c] = function()
{
   this.do_neg();
};
// 0x7d : RETN
Z80.prototype.ed_instructions[0x7d] = function()
{
   this.pc = (this.pop_word() - 1) & 0xffff;
   this.iff1 = this.iff2;
};
// 0x7e : IM 2
Z80.prototype.ed_instructions[0x7e] = function()
{
   this.imode = 2;
};
// 0xa0 : LDI
Z80.prototype.ed_instructions[0xa0] = function()
{
   this.do_ldi();
};
// 0xa1 : CPI
Z80.prototype.ed_instructions[0xa1] = function()
{
   this.do_cpi();
};
// 0xa2 : INI
Z80.prototype.ed_instructions[0xa2] = function()
{
   this.do_ini();
};
// 0xa3 : OUTI
Z80.prototype.ed_instructions[0xa3] = function()
{
   this.do_outi();
};
// 0xa8 : LDD
Z80.prototype.ed_instructions[0xa8] = function()
{
   this.do_ldd();
};
// 0xa9 : CPD
Z80.prototype.ed_instructions[0xa9] = function()
{
   this.do_cpd();
};
// 0xaa : IND
Z80.prototype.ed_instructions[0xaa] = function()
{
   this.do_ind();
};
// 0xab : OUTD
Z80.prototype.ed_instructions[0xab] = function()
{
   this.do_outd();
};
// 0xb0 : LDIR
Z80.prototype.ed_instructions[0xb0] = function()
{
   this.do_ldi();
   if (this.b || this.c)
   {
      this.cycle_counter += 5;
      this.pc = (this.pc - 2) & 0xffff;
   }
};
// 0xb1 : CPIR
Z80.prototype.ed_instructions[0xb1] = function()
{
   this.do_cpi();
   if (!this.flags.Z && (this.b || this.c))
   {
      this.cycle_counter += 5;
      this.pc = (this.pc - 2) & 0xffff;
   }
};
// 0xb2 : INIR
Z80.prototype.ed_instructions[0xb2] = function()
{
   this.do_ini();
   if (this.b)
   {
      this.cycle_counter += 5;
      this.pc = (this.pc - 2) & 0xffff;
   }
};
// 0xb3 : OTIR
Z80.prototype.ed_instructions[0xb3] = function()
{
   this.do_outi();
   if (this.b)
   {
      this.cycle_counter += 5;
      this.pc = (this.pc - 2) & 0xffff;
   }
};
// 0xb8 : LDDR
Z80.prototype.ed_instructions[0xb8] = function()
{
   this.do_ldd();
   if (this.b || this.c)
   {
      this.cycle_counter += 5;
      this.pc = (this.pc - 2) & 0xffff;
   }
};
// 0xb9 : CPDR
Z80.prototype.ed_instructions[0xb9] = function()
{
   this.do_cpd();
   if (!this.flags.Z && (this.b || this.c))
   {
      this.cycle_counter += 5;
      this.pc = (this.pc - 2) & 0xffff;
   }
};
// 0xba : INDR
Z80.prototype.ed_instructions[0xba] = function()
{
   this.do_ind();
   if (this.b)
   {
      this.cycle_counter += 5;
      this.pc = (this.pc - 2) & 0xffff;
   }
};
// 0xbb : OTDR
Z80.prototype.ed_instructions[0xbb] = function()
{
   this.do_outd();
   if (this.b)
   {
      this.cycle_counter += 5;
      this.pc = (this.pc - 2) & 0xffff;
   }
};


///////////////////////////////////////////////////////////////////////////////
/// Like ED, this table is quite sparse,
///  and many of the opcodes here are also undocumented.
/// The undocumented instructions here are those that deal with only one byte
///  of the two-byte IX register; the bytes are designed IXH and IXL here.
///////////////////////////////////////////////////////////////////////////////
Z80.prototype.dd_instructions = [];
// 0x09 : ADD IX, BC
Z80.prototype.dd_instructions[0x09] = function()
{
   this.do_ix_add(this.c | (this.b << 8));
};
// 0x19 : ADD IX, DE
Z80.prototype.dd_instructions[0x19] = function()
{
   this.do_ix_add(this.e | (this.d << 8));
};
// 0x21 : LD IX, nn
Z80.prototype.dd_instructions[0x21] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   this.ix = this.core.mem_read(this.pc);
   this.pc = (this.pc + 1) & 0xffff;
   this.ix |= (this.core.mem_read(this.pc) << 8);
};
// 0x22 : LD (nn), IX
Z80.prototype.dd_instructions[0x22] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   var address = this.core.mem_read(this.pc);
   this.pc = (this.pc + 1) & 0xffff;
   address |= (this.core.mem_read(this.pc) << 8);
   
   this.core.mem_write(address, this.ix & 0xff);
   this.core.mem_write((address + 1) & 0xffff, (this.ix >>> 8) & 0xff);
};
// 0x23 : INC IX
Z80.prototype.dd_instructions[0x23] = function()
{
   this.ix = (this.ix + 1) & 0xffff;
};
// 0x24 : INC IXH (Undocumented)
Z80.prototype.dd_instructions[0x24] = function()
{
   this.ix = (this.do_inc(this.ix >>> 8) << 8) | (this.ix & 0xff);
};
// 0x25 : DEC IXH (Undocumented)
Z80.prototype.dd_instructions[0x25] = function()
{
   this.ix = (this.do_dec(this.ix >>> 8) << 8) | (this.ix & 0xff);
};
// 0x26 : LD IXH, n (Undocumented)
Z80.prototype.dd_instructions[0x26] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   this.ix = (this.core.mem_read(this.pc) << 8) | (this.ix & 0xff);
};
// 0x29 : ADD IX, IX
Z80.prototype.dd_instructions[0x29] = function()
{
   this.do_ix_add(this.ix);
};
// 0x2a : LD IX, (nn)
Z80.prototype.dd_instructions[0x2a] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   var address = this.core.mem_read(this.pc);
   this.pc = (this.pc + 1) & 0xffff;
   address |= (this.core.mem_read(this.pc) << 8);
   
   this.ix = this.core.mem_read(address);
   this.ix |= (this.core.mem_read((address + 1) & 0xffff) << 8);
};
// 0x2b : DEC IX
Z80.prototype.dd_instructions[0x2b] = function()
{
   this.ix = (this.ix - 1) & 0xffff;
};
// 0x2c : INC IXL (Undocumented)
Z80.prototype.dd_instructions[0x2c] = function()
{
   this.ix = this.do_inc(this.ix & 0xff) | (this.ix & 0xff00);
};
// 0x2d : DEC IXL (Undocumented)
Z80.prototype.dd_instructions[0x2d] = function()
{
   this.ix = this.do_dec(this.ix & 0xff) | (this.ix & 0xff00);
};
// 0x2e : LD IXL, n (Undocumented)
Z80.prototype.dd_instructions[0x2e] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   this.ix = (this.core.mem_read(this.pc) & 0xff) | (this.ix & 0xff00);
};
// 0x34 : INC (IX+n)
Z80.prototype.dd_instructions[0x34] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   var offset = this.get_signed_offset_byte(this.core.mem_read(this.pc)),
       value = this.core.mem_read((offset + this.ix) & 0xffff);
   this.core.mem_write((offset + this.ix) & 0xffff, this.do_inc(value));
};
// 0x35 : DEC (IX+n)
Z80.prototype.dd_instructions[0x35] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   var offset = this.get_signed_offset_byte(this.core.mem_read(this.pc)),
       value = this.core.mem_read((offset + this.ix) & 0xffff);
   this.core.mem_write((offset + this.ix) & 0xffff, this.do_dec(value));
};
// 0x36 : LD (IX+n), n
Z80.prototype.dd_instructions[0x36] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   var offset = this.get_signed_offset_byte(this.core.mem_read(this.pc));
   this.pc = (this.pc + 1) & 0xffff;
   this.core.mem_write((this.ix + offset) & 0xffff, this.core.mem_read(this.pc));   
};
// 0x39 : ADD IX, SP
Z80.prototype.dd_instructions[0x39] = function()
{
   this.do_ix_add(this.sp);
};
// 0x44 : LD B, IXH (Undocumented)
Z80.prototype.dd_instructions[0x44] = function()
{
   this.b = (this.ix >>> 8) & 0xff;
};
// 0x45 : LD B, IXL (Undocumented)
Z80.prototype.dd_instructions[0x45] = function()
{
   this.b = this.ix & 0xff;
};
// 0x46 : LD B, (IX+n)
Z80.prototype.dd_instructions[0x46] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   var offset = this.get_signed_offset_byte(this.core.mem_read(this.pc));
   this.b = this.core.mem_read((this.ix + offset) & 0xffff);
};
// 0x4c : LD C, IXH (Undocumented)
Z80.prototype.dd_instructions[0x4c] = function()
{
   this.c = (this.ix >>> 8) & 0xff;
};
// 0x4d : LD C, IXL (Undocumented)
Z80.prototype.dd_instructions[0x4d] = function()
{
   this.c = this.ix & 0xff;
};
// 0x4e : LD C, (IX+n)
Z80.prototype.dd_instructions[0x4e] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   var offset = this.get_signed_offset_byte(this.core.mem_read(this.pc));
   this.c = this.core.mem_read((this.ix + offset) & 0xffff);
};
// 0x54 : LD D, IXH (Undocumented)
Z80.prototype.dd_instructions[0x54] = function()
{
   this.d = (this.ix >>> 8) & 0xff;
};
// 0x55 : LD D, IXL (Undocumented)
Z80.prototype.dd_instructions[0x55] = function()
{
   this.d = this.ix & 0xff;
};
// 0x56 : LD D, (IX+n)
Z80.prototype.dd_instructions[0x56] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   var offset = this.get_signed_offset_byte(this.core.mem_read(this.pc));
   this.d = this.core.mem_read((this.ix + offset) & 0xffff);
};
// 0x5c : LD E, IXH (Undocumented)
Z80.prototype.dd_instructions[0x5c] = function()
{
   this.e = (this.ix >>> 8) & 0xff;
};
// 0x5d : LD E, IXL (Undocumented)
Z80.prototype.dd_instructions[0x5d] = function()
{
   this.e = this.ix & 0xff;
};
// 0x5e : LD E, (IX+n)
Z80.prototype.dd_instructions[0x5e] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   var offset = this.get_signed_offset_byte(this.core.mem_read(this.pc));
   this.e = this.core.mem_read((this.ix + offset) & 0xffff);
};
// 0x60 : LD IXH, B (Undocumented)
Z80.prototype.dd_instructions[0x60] = function()
{
   this.ix = (this.ix & 0xff) | (this.b << 8);
};
// 0x61 : LD IXH, C (Undocumented)
Z80.prototype.dd_instructions[0x61] = function()
{
   this.ix = (this.ix & 0xff) | (this.c << 8);
};
// 0x62 : LD IXH, D (Undocumented)
Z80.prototype.dd_instructions[0x62] = function()
{
   this.ix = (this.ix & 0xff) | (this.d << 8);
};
// 0x63 : LD IXH, E (Undocumented)
Z80.prototype.dd_instructions[0x63] = function()
{
   this.ix = (this.ix & 0xff) | (this.e << 8);
};
// 0x64 : LD IXH, IXH (Undocumented)
Z80.prototype.dd_instructions[0x64] = function()
{
   // No-op.
};
// 0x65 : LD IXH, IXL (Undocumented)
Z80.prototype.dd_instructions[0x65] = function()
{
   this.ix = (this.ix & 0xff) | ((this.ix & 0xff) << 8);
};
// 0x66 : LD H, (IX+n)
Z80.prototype.dd_instructions[0x66] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   var offset = this.get_signed_offset_byte(this.core.mem_read(this.pc));
   this.h = this.core.mem_read((this.ix + offset) & 0xffff);
};
// 0x67 : LD IXH, A (Undocumented)
Z80.prototype.dd_instructions[0x67] = function()
{
   this.ix = (this.ix & 0xff) | (this.a << 8);
};
// 0x68 : LD IXL, B (Undocumented)
Z80.prototype.dd_instructions[0x68] = function()
{
   this.ix = (this.ix & 0xff00) | this.b;
};
// 0x69 : LD IXL, C (Undocumented)
Z80.prototype.dd_instructions[0x69] = function()
{
   this.ix = (this.ix & 0xff00) | this.c;
};
// 0x6a : LD IXL, D (Undocumented)
Z80.prototype.dd_instructions[0x6a] = function()
{
   this.ix = (this.ix & 0xff00) | this.d;
};
// 0x6b : LD IXL, E (Undocumented)
Z80.prototype.dd_instructions[0x6b] = function()
{
   this.ix = (this.ix & 0xff00) | this.e;
};
// 0x6c : LD IXL, IXH (Undocumented)
Z80.prototype.dd_instructions[0x6c] = function()
{
   this.ix = (this.ix & 0xff00) | (this.ix >>> 8);
};
// 0x6d : LD IXL, IXL (Undocumented)
Z80.prototype.dd_instructions[0x6d] = function()
{
   // No-op.
};
// 0x6e : LD L, (IX+n)
Z80.prototype.dd_instructions[0x6e] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   var offset = this.get_signed_offset_byte(this.core.mem_read(this.pc));
   this.l = this.core.mem_read((this.ix + offset) & 0xffff);
};
// 0x6f : LD IXL, A (Undocumented)
Z80.prototype.dd_instructions[0x6f] = function()
{
   this.ix = (this.ix & 0xff00) | this.a;
};
// 0x70 : LD (IX+n), B
Z80.prototype.dd_instructions[0x70] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   var offset = this.get_signed_offset_byte(this.core.mem_read(this.pc));
   this.core.mem_write((this.ix + offset) & 0xffff, this.b);
};
// 0x71 : LD (IX+n), C
Z80.prototype.dd_instructions[0x71] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   var offset = this.get_signed_offset_byte(this.core.mem_read(this.pc));
   this.core.mem_write((this.ix + offset) & 0xffff, this.c);
};
// 0x72 : LD (IX+n), D
Z80.prototype.dd_instructions[0x72] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   var offset = this.get_signed_offset_byte(this.core.mem_read(this.pc));
   this.core.mem_write((this.ix + offset) & 0xffff, this.d);
};
// 0x73 : LD (IX+n), E
Z80.prototype.dd_instructions[0x73] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   var offset = this.get_signed_offset_byte(this.core.mem_read(this.pc));
   this.core.mem_write((this.ix + offset) & 0xffff, this.e);
};
// 0x74 : LD (IX+n), H
Z80.prototype.dd_instructions[0x74] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   var offset = this.get_signed_offset_byte(this.core.mem_read(this.pc));
   this.core.mem_write((this.ix + offset) & 0xffff, this.h);
};
// 0x75 : LD (IX+n), L
Z80.prototype.dd_instructions[0x75] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   var offset = this.get_signed_offset_byte(this.core.mem_read(this.pc));
   this.core.mem_write((this.ix + offset) & 0xffff, this.l);
};
// 0x77 : LD (IX+n), A
Z80.prototype.dd_instructions[0x77] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   var offset = this.get_signed_offset_byte(this.core.mem_read(this.pc));
   this.core.mem_write((this.ix + offset) & 0xffff, this.a);
};
// 0x7c : LD A, IXH (Undocumented)
Z80.prototype.dd_instructions[0x7c] = function()
{
   this.a = (this.ix >>> 8) & 0xff;
};
// 0x7d : LD A, IXL (Undocumented)
Z80.prototype.dd_instructions[0x7d] = function()
{
   this.a = this.ix & 0xff;
};
// 0x7e : LD A, (IX+n)
Z80.prototype.dd_instructions[0x7e] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   var offset = this.get_signed_offset_byte(this.core.mem_read(this.pc));
   this.a = this.core.mem_read((this.ix + offset) & 0xffff);
};
// 0x84 : ADD A, IXH (Undocumented)
Z80.prototype.dd_instructions[0x84] = function()
{
   this.do_add((this.ix >>> 8) & 0xff);
};
// 0x85 : ADD A, IXL (Undocumented)
Z80.prototype.dd_instructions[0x85] = function()
{
   this.do_add(this.ix & 0xff);
};
// 0x86 : ADD A, (IX+n)
Z80.prototype.dd_instructions[0x86] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   var offset = this.get_signed_offset_byte(this.core.mem_read(this.pc));
   this.do_add(this.core.mem_read((this.ix + offset) & 0xffff));
};
// 0x8c : ADC A, IXH (Undocumented)
Z80.prototype.dd_instructions[0x8c] = function()
{
   this.do_adc((this.ix >>> 8) & 0xff);
};
// 0x8d : ADC A, IXL (Undocumented)
Z80.prototype.dd_instructions[0x8d] = function()
{
   this.do_adc(this.ix & 0xff);
};
// 0x8e : ADC A, (IX+n)
Z80.prototype.dd_instructions[0x8e] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   var offset = this.get_signed_offset_byte(this.core.mem_read(this.pc));
   this.do_adc(this.core.mem_read((this.ix + offset) & 0xffff));
};
// 0x94 : SUB IXH (Undocumented)
Z80.prototype.dd_instructions[0x94] = function()
{
   this.do_sub((this.ix >>> 8) & 0xff);
};
// 0x95 : SUB IXL (Undocumented)
Z80.prototype.dd_instructions[0x95] = function()
{
   this.do_sub(this.ix & 0xff);
};
// 0x96 : SUB A, (IX+n)
Z80.prototype.dd_instructions[0x96] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   var offset = this.get_signed_offset_byte(this.core.mem_read(this.pc));
   this.do_sub(this.core.mem_read((this.ix + offset) & 0xffff));
};
// 0x9c : SBC IXH (Undocumented)
Z80.prototype.dd_instructions[0x9c] = function()
{
   this.do_sbc((this.ix >>> 8) & 0xff);
};
// 0x9d : SBC IXL (Undocumented)
Z80.prototype.dd_instructions[0x9d] = function()
{
   this.do_sbc(this.ix & 0xff);
};
// 0x9e : SBC A, (IX+n)
Z80.prototype.dd_instructions[0x9e] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   var offset = this.get_signed_offset_byte(this.core.mem_read(this.pc));
   this.do_sbc(this.core.mem_read((this.ix + offset) & 0xffff));
};
// 0xa4 : AND IXH (Undocumented)
Z80.prototype.dd_instructions[0xa4] = function()
{
   this.do_and((this.ix >>> 8) & 0xff);
};
// 0xa5 : AND IXL (Undocumented)
Z80.prototype.dd_instructions[0xa5] = function()
{
   this.do_and(this.ix & 0xff);
};
// 0xa6 : AND A, (IX+n)
Z80.prototype.dd_instructions[0xa6] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   var offset = this.get_signed_offset_byte(this.core.mem_read(this.pc));
   this.do_and(this.core.mem_read((this.ix + offset) & 0xffff));
};
// 0xac : XOR IXH (Undocumented)
Z80.prototype.dd_instructions[0xac] = function()
{
   this.do_xor((this.ix >>> 8) & 0xff);
};
// 0xad : XOR IXL (Undocumented)
Z80.prototype.dd_instructions[0xad] = function()
{
   this.do_xor(this.ix & 0xff);
};
// 0xae : XOR A, (IX+n)
Z80.prototype.dd_instructions[0xae] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   var offset = this.get_signed_offset_byte(this.core.mem_read(this.pc));
   this.do_xor(this.core.mem_read((this.ix + offset) & 0xffff));
};
// 0xb4 : OR IXH (Undocumented)
Z80.prototype.dd_instructions[0xb4] = function()
{
   this.do_or((this.ix >>> 8) & 0xff);
};
// 0xb5 : OR IXL (Undocumented)
Z80.prototype.dd_instructions[0xb5] = function()
{
   this.do_or(this.ix & 0xff);
};
// 0xb6 : OR A, (IX+n)
Z80.prototype.dd_instructions[0xb6] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   var offset = this.get_signed_offset_byte(this.core.mem_read(this.pc));
   this.do_or(this.core.mem_read((this.ix + offset) & 0xffff));
};
// 0xbc : CP IXH (Undocumented)
Z80.prototype.dd_instructions[0xbc] = function()
{
   this.do_cp((this.ix >>> 8) & 0xff);
};
// 0xbd : CP IXL (Undocumented)
Z80.prototype.dd_instructions[0xbd] = function()
{
   this.do_cp(this.ix & 0xff);
};
// 0xbe : CP A, (IX+n)
Z80.prototype.dd_instructions[0xbe] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   var offset = this.get_signed_offset_byte(this.core.mem_read(this.pc));
   this.do_cp(this.core.mem_read((this.ix + offset) & 0xffff));
};
// 0xcb : CB Prefix (IX bit instructions)
Z80.prototype.dd_instructions[0xcb] = function()
{
   this.pc = (this.pc + 1) & 0xffff;
   var offset = this.get_signed_offset_byte(this.core.mem_read(this.pc));
   this.pc = (this.pc + 1) & 0xffff;
   var opcode = this.core.mem_read(this.pc), value;
   
   // As with the "normal" CB prefix, we implement the DDCB prefix
   //  by decoding the opcode directly, rather than using a table.
   if (opcode < 0x40)
   {
      // Shift and rotate instructions.
      var ddcb_functions = [this.do_rlc, this.do_rrc, this.do_rl, this.do_rr,
                            this.do_sla, this.do_sra, this.do_sll, this.do_srl];
      
      // Most of the opcodes in this range are not valid,
      //  so we map this opcode onto one of the ones that is.
      var func = ddcb_functions[(opcode & 0x38) >>> 3],
      value = func.call(this, this.core.mem_read((this.ix + offset) & 0xffff));
      
      this.core.mem_write((this.ix + offset) & 0xffff, value);
   }
   else
   {
      var bit_number = (opcode & 0x38) >>> 3;
      
      if (opcode < 0x80)
      {
         // BIT
         this.flags.N = 0;
         this.flags.H = 1;
         this.flags.Z = !(this.core.mem_read((this.ix + offset) & 0xffff) & (1 << bit_number)) ? 1 : 0;
         this.flags.P = this.flags.Z;
         this.flags.S = ((bit_number === 7) && !this.flags.Z) ? 1 : 0;
      }
      else if (opcode < 0xc0)
      {
         // RES
         value = this.core.mem_read((this.ix + offset) & 0xffff) & ~(1 << bit_number) & 0xff;
         this.core.mem_write((this.ix + offset) & 0xffff, value);
      }
      else
      {
         // SET
         value = this.core.mem_read((this.ix + offset) & 0xffff) | (1 << bit_number);
         this.core.mem_write((this.ix + offset) & 0xffff, value);
      }
   }
   
   // This implements the undocumented shift, RES, and SET opcodes,
   //  which write their result to memory and also to an 8080 register.
   if (value !== undefined)
   {
      if ((opcode & 0x07) === 0)
         this.b = value;
      else if ((opcode & 0x07) === 1)
         this.c = value;
      else if ((opcode & 0x07) === 2)
         this.d = value;
      else if ((opcode & 0x07) === 3)
         this.e = value;
      else if ((opcode & 0x07) === 4)
         this.h = value;
      else if ((opcode & 0x07) === 5)
         this.l = value;
      // 6 is the documented opcode, which doesn't set a register.
      else if ((opcode & 0x07) === 7)
         this.a = value;
   }
   
   this.cycle_counter += this.cycle_counts_cb[opcode] + 8;
};
// 0xe1 : POP IX
Z80.prototype.dd_instructions[0xe1] = function()
{
   this.ix = this.pop_word();
};
// 0xe3 : EX (SP), IX
Z80.prototype.dd_instructions[0xe3] = function()
{
   var temp = this.ix;
   this.ix = this.core.mem_read(this.sp);
   this.ix |= this.core.mem_read((this.sp + 1) & 0xffff) << 8;
   this.core.mem_write(this.sp, temp & 0xff);
   this.core.mem_write((this.sp + 1) & 0xffff, (temp >>> 8) & 0xff);
};
// 0xe5 : PUSH IX
Z80.prototype.dd_instructions[0xe5] = function()
{
   this.push_word(this.ix);
};
// 0xe9 : JP (IX)
Z80.prototype.dd_instructions[0xe9] = function()
{
   this.pc = (this.ix - 1) & 0xffff;
};
// 0xf9 : LD SP, IX
Z80.prototype.dd_instructions[0xf9] = function()
{
   this.sp = this.ix;
};


///////////////////////////////////////////////////////////////////////////////
/// These tables contain the number of T cycles used for each instruction.
/// In a few special cases, such as conditional control flow instructions,
///  additional cycles might be added to these values.
/// The total number of cycles is the return value of run_instruction().
///////////////////////////////////////////////////////////////////////////////
Z80.prototype.cycle_counts = [
    4, 10,  7,  6,  4,  4,  7,  4,  4, 11,  7,  6,  4,  4,  7,  4,
    8, 10,  7,  6,  4,  4,  7,  4, 12, 11,  7,  6,  4,  4,  7,  4,
    7, 10, 16,  6,  4,  4,  7,  4,  7, 11, 16,  6,  4,  4,  7,  4,
    7, 10, 13,  6, 11, 11, 10,  4,  7, 11, 13,  6,  4,  4,  7,  4,
    4,  4,  4,  4,  4,  4,  7,  4,  4,  4,  4,  4,  4,  4,  7,  4,
    4,  4,  4,  4,  4,  4,  7,  4,  4,  4,  4,  4,  4,  4,  7,  4,
    4,  4,  4,  4,  4,  4,  7,  4,  4,  4,  4,  4,  4,  4,  7,  4,
    7,  7,  7,  7,  7,  7,  4,  7,  4,  4,  4,  4,  4,  4,  7,  4,
    4,  4,  4,  4,  4,  4,  7,  4,  4,  4,  4,  4,  4,  4,  7,  4,
    4,  4,  4,  4,  4,  4,  7,  4,  4,  4,  4,  4,  4,  4,  7,  4,
    4,  4,  4,  4,  4,  4,  7,  4,  4,  4,  4,  4,  4,  4,  7,  4,
    4,  4,  4,  4,  4,  4,  7,  4,  4,  4,  4,  4,  4,  4,  7,  4,
    5, 10, 10, 10, 10, 11,  7, 11,  5, 10, 10,  0, 10, 17,  7, 11,
    5, 10, 10, 11, 10, 11,  7, 11,  5,  4, 10, 11, 10,  0,  7, 11,
    5, 10, 10, 19, 10, 11,  7, 11,  5,  4, 10,  4, 10,  0,  7, 11,
    5, 10, 10,  4, 10, 11,  7, 11,  5,  4, 10,  4, 10,  0,  7, 11
];

Z80.prototype.cycle_counts_ed = [
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
   12, 12, 15, 20,  8, 14,  8,  9, 12, 12, 15, 20,  8, 14,  8,  9,
   12, 12, 15, 20,  8, 14,  8,  9, 12, 12, 15, 20,  8, 14,  8,  9,
   12, 12, 15, 20,  8, 14,  8, 18, 12, 12, 15, 20,  8, 14,  8, 18,
   12, 12, 15, 20,  8, 14,  8,  0, 12, 12, 15, 20,  8, 14,  8,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
   16, 16, 16, 16,  0,  0,  0,  0, 16, 16, 16, 16,  0,  0,  0,  0,
   16, 16, 16, 16,  0,  0,  0,  0, 16, 16, 16, 16,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
];

Z80.prototype.cycle_counts_cb = [
    8,  8,  8,  8,  8,  8, 15,  8,  8,  8,  8,  8,  8,  8, 15,  8,
    8,  8,  8,  8,  8,  8, 15,  8,  8,  8,  8,  8,  8,  8, 15,  8,
    8,  8,  8,  8,  8,  8, 15,  8,  8,  8,  8,  8,  8,  8, 15,  8,
    8,  8,  8,  8,  8,  8, 15,  8,  8,  8,  8,  8,  8,  8, 15,  8,
    8,  8,  8,  8,  8,  8, 12,  8,  8,  8,  8,  8,  8,  8, 12,  8,
    8,  8,  8,  8,  8,  8, 12,  8,  8,  8,  8,  8,  8,  8, 12,  8,
    8,  8,  8,  8,  8,  8, 12,  8,  8,  8,  8,  8,  8,  8, 12,  8,
    8,  8,  8,  8,  8,  8, 12,  8,  8,  8,  8,  8,  8,  8, 12,  8,
    8,  8,  8,  8,  8,  8, 15,  8,  8,  8,  8,  8,  8,  8, 15,  8,
    8,  8,  8,  8,  8,  8, 15,  8,  8,  8,  8,  8,  8,  8, 15,  8,
    8,  8,  8,  8,  8,  8, 15,  8,  8,  8,  8,  8,  8,  8, 15,  8,
    8,  8,  8,  8,  8,  8, 15,  8,  8,  8,  8,  8,  8,  8, 15,  8,
    8,  8,  8,  8,  8,  8, 15,  8,  8,  8,  8,  8,  8,  8, 15,  8,
    8,  8,  8,  8,  8,  8, 15,  8,  8,  8,  8,  8,  8,  8, 15,  8,
    8,  8,  8,  8,  8,  8, 15,  8,  8,  8,  8,  8,  8,  8, 15,  8,
    8,  8,  8,  8,  8,  8, 15,  8,  8,  8,  8,  8,  8,  8, 15,  8
];

Z80.prototype.cycle_counts_dd = [
    0,  0,  0,  0,  0,  0,  0,  0,  0, 15,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0, 15,  0,  0,  0,  0,  0,  0,
    0, 14, 20, 10,  8,  8, 11,  0,  0, 15, 20, 10,  8,  8, 11,  0,
    0,  0,  0,  0, 23, 23, 19,  0,  0, 15,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  8,  8, 19,  0,  0,  0,  0,  0,  8,  8, 19,  0,
    0,  0,  0,  0,  8,  8, 19,  0,  0,  0,  0,  0,  8,  8, 19,  0,
    8,  8,  8,  8,  8,  8, 19,  8,  8,  8,  8,  8,  8,  8, 19,  8,
   19, 19, 19, 19, 19, 19,  0, 19,  0,  0,  0,  0,  8,  8, 19,  0,
    0,  0,  0,  0,  8,  8, 19,  0,  0,  0,  0,  0,  8,  8, 19,  0,
    0,  0,  0,  0,  8,  8, 19,  0,  0,  0,  0,  0,  8,  8, 19,  0,
    0,  0,  0,  0,  8,  8, 19,  0,  0,  0,  0,  0,  8,  8, 19,  0,
    0,  0,  0,  0,  8,  8, 19,  0,  0,  0,  0,  0,  8,  8, 19,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0, 14,  0, 23,  0, 15,  0,  0,  0,  8,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0, 10,  0,  0,  0,  0,  0,  0
];
