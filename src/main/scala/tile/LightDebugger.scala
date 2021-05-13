// Debugging Accelerator for the Nexys A7, currently connects to the 7-segment display
// (c) Maddie Burbage, 2021, for the Bailey Research Group at Williams

package freechips.rocketchip.tile

import Chisel._
import chisel3.util.{Enum, log2Ceil, UIntToOH}
import chisel3.VecInit
import freechips.rocketchip.config._ //For Config
import freechips.rocketchip.diplomacy._ //For LazyModule
import freechips.rocketchip.rocket.{HellaCacheReq} //For cache access

// Wrapper for the accelerator
class LightDebugger()(implicit p: Parameters) extends LazyRoCC {
    override lazy val module = new LightDebuggerImp(this)
}


// Main accelerator class, connects memory-access data to the light debugger interface
class LightDebuggerImp(outer: LightDebugger)(implicit p: Parameters) extends LazyRoCCModule(outer){

  // States
  val s_idle :: s_busy :: s_wait :: s_resp :: Nil = Enum(4)
  val state = Reg(init = s_idle) //Idle until handling an instruction
  io.interrupt := false.B

  /** RoCC Handling **/
  // Instruction inputs
  val address = Reg(init = 0.U(64.W))
  val data = Reg(init = 0.U(64.W))
  val typ = Reg(init = 0.U(2.W)) //Can be 3 wide but we don't have room in inst
  val command = Reg(init = 0.U(5.W))
  val rd = Reg(init = 0.U(5.W))
  val returned = Reg(init = 0.U(64.W))

  //RoCC interface
  io.cmd.ready := (state === s_idle)
  io.resp.valid := (state === s_resp)
  io.busy := (state === s_busy) || (state === s_wait)
  io.resp.bits.data := returned
  io.resp.bits.rd := rd

  //Memory request interface
  io.mem.req.valid := (state === s_busy)
  io.mem.req.bits.addr := address
  io.mem.req.bits.tag :=  address(5,0)
  io.mem.req.bits.cmd := command
  io.mem.req.bits.data := data
  io.mem.req.bits.typ := typ
  io.mem.req.bits.phys := Bool(false)

  /** Display Handling **/
  val width = 8 // How many digits of the display to use
  val numberWidth = 6 // How many digits hold numeric values
  val encoding = DisplayDriver.basicHex // What encoding type is used for the display
  val bitWidth = log2Ceil(encoding.length) // The bits needed for a single numeric digit
  val number = RegInit(VecInit( Seq.fill(numberWidth) (0.U(bitWidth.W)) )) // The numeric data, stored in binary
  val pure = RegInit(VecInit( Seq.fill(width - numberWidth) (0.U(8.W)) )) // The on-numeric data, stored in binary
  val driver = Module(new DisplayDriver(width, encoding, numberWidth)) // Controls for the digit display
  
  driver.io.number := number
  driver.io.pure := pure
  io.lights <> driver.io

  /** State Transitions **/
  //Once an instruction is received, capture data and prepare memory request
  when(io.cmd.fire()) {
    state := s_busy

    address := io.cmd.bits.rs1
    data := io.cmd.bits.rs2
    typ := io.cmd.bits.inst.funct(6,5)
    command := io.cmd.bits.inst.funct(4,0)
    rd := io.cmd.bits.inst.rd
  }

  //Once a memory request is fired, wait
  when(io.mem.req.fire()) {
    state := s_wait
  }

  //When a memory response is received, capture data and prepare cpu response
  when(io.mem.resp.valid && state === s_wait) {
    state := s_resp
    assert(io.mem.resp.bits.addr === address)
    assert(io.mem.resp.bits.tag === address(5,0))

    returned := io.mem.resp.bits.data
  }

  //When an exception comes, respond with an error to the cpu
  when(state === s_wait) {
    when(io.mem.s2_xcpt.asUInt.orR) {
      state := s_resp
      returned := (0.U(64.W) - io.mem.s2_xcpt.asUInt)
    }
  }

  //Once a cpu response is sent, wait for new instructions
  when(io.resp.fire()) {
    state := s_idle
  }

  /** Light Debugger Event Handling **/
  // Many of these memory signals will stay at 0 unless Maddie's SimpleHellaCacheIF modifications are followed
  // For more detailed information on an event, capture the state or current cycle when an event happens, or count the number of events

  // Collect performance events
  val perfEvents = Seq(io.mem.perf.acquire.asUInt(), io.mem.perf.release.asUInt(), io.mem.perf.tlbMiss.asUInt())
  
  // Track single-cycle events since the last memory request
  val hasNacked = RegInit(false.B)
  val hasMaLoad = RegInit(false.B)
  val hasMaStore = RegInit(false.B)
  val hasPfLoad = RegInit(false.B)
  val hasPfStore = RegInit(false.B)
  val hasAeLoad = RegInit(false.B)
  val hasAeStore = RegInit(false.B)

  when(io.mem.s2_nack) {
    hasNacked := true.B
  }
  
  when(io.mem.s2_xcpt.ma.ld) {
    hasMaLoad := true.B
  }
  when(io.mem.s2_xcpt.ma.st) {
    hasMaStore := true.B
  }
  when(io.mem.s2_xcpt.pf.ld) {
    hasPfLoad := true.B
  }
  when(io.mem.s2_xcpt.pf.st) {
    hasPfStore := true.B
  }
  when(io.mem.s2_xcpt.ae.ld) {
    hasAeLoad := true.B
  }
  when(io.mem.s2_xcpt.ae.st) {
    hasAeStore := true.B
  }

  // Once a request has been sent, reset the captured exception signals
  when(io.mem.req.fire()) {
    hasNacked := false.B
    hasMaLoad := false.B
    hasMaStore := false.B
    hasPfLoad := false.B
    hasPfStore := false.B
    hasAeLoad := false.B
    hasAeStore := false.B
  }

  // The non-numeric data to be displayed (on the left-hand-side digits of the display)
  pure(0) := io.mem.resp.valid ## io.mem.req.fire() ## io.mem.req.valid ## io.busy ## perfEvents.reduce(_ ## _)
  pure(1) := hasAeStore ## hasAeLoad ## hasPfStore ## hasPfLoad ## hasMaStore ## hasMaLoad

  // The numeric data to be displayed (on the right-hand-side digits of the display)
  when(io.mem.req.fire()) {
    number(0) := io.mem.req.bits.addr(3,0)
    number(1) := io.mem.req.bits.addr(7,4)
    number(2) := io.mem.req.bits.data(3,0)
  }
  when(io.mem.resp.valid) {
    number(3) := io.mem.resp.bits.addr(3,0)
    number(4) := io.mem.resp.bits.addr(7,4)
    number(5) := io.mem.resp.bits.data(3,0)
  }
}


// Controls the values of the digit display
class DisplayDriver(width: Int, encoding: List[Int], numberWidth: Int) extends Module {
  require(numberWidth <= width) //The width of any numbers shouldn't be bigger than the total width

  val bitWidth = log2Ceil(encoding.length) // The bits needed for a single digit

  val io = IO(new Bundle {
    val number = Vec(numberWidth, UInt(INPUT, bitWidth.W))
    val pure = Vec((width - numberWidth), UInt(INPUT, 8.W))
    val AN = UInt(OUTPUT, width = 8.W)
    val CA = UInt(OUTPUT, width = 8.W)
  })

  // Values to be shown on the display
  val display = RegInit(VecInit( Seq.fill(width) (0.U(8.W)) )) 

  // For each digit, convert binary data into the segments used
  val mappings = RegInit(VecInit.tabulate(encoding.length)(n => (encoding(n)).U(8.W)))

  // From lowest to highest (left to right) map each number digit to a display digit
  for((n, i) <- io.number.view.zipWithIndex) {
    display(i) := mappings(n)
  }

  // Map non-encoded digits to the rest of the display digits
  for((n, i) <- io.pure.view.zipWithIndex) {
    val offset = i + numberWidth
    display(offset) := n
  }
  
  // Display a different digit every 500 Hz
  val refresh = Module(new ClockHz(500))
  val refreshCycle = DisplayDriver.getPulse(refresh.io.clk)
  val marker = RegInit(0.U(log2Ceil(width).W))

  when(refreshCycle) {
    marker := Mux(marker === (width - 1).U, 0.U, marker + 1.U)
  }

  // Connect our data to the anodes and cathodes - the active display segments must be driven low
  io.AN := ~UIntToOH(marker)
  io.CA := ~display(marker)
}

/** Digit Display helper tools */
object DisplayDriver {

  // The cathodes, which control segments (7 segments and 1 period)
  val ca :: cb :: cc :: cd :: ce :: cf :: cg :: dp :: Nil =
    (0 until 8).map(n => (1 << n)).toList

  // Cathode choices for hexadecimal digits
  lazy val basicHex = (ca | cb | cc | cd | ce | cf) :: //0
  (cb | cc) :: //1
  (ca | cb | cg | ce | cd) :: //2
  (ca | cb | cg | cc | cd) :: //3
  (cf | cg | cb | cc) :: //4
  (ca | cf | cg | cc | cd) :: //5
  (ca | cf | cg | cc | cd | ce) :: //6
  (ca | cb | cc) :: //7
  (ca | cb | cc | cd | ce | cf | cg) :: //8
  (ca | cb | cg | cf | cc) :: //9
  (ca | cb | cg | cc | cd | ce) :: //a
  (cg | cc | cd | ce | cf) :: //b
  (cg | cd | ce) :: //c
  (cb | cc | cd | ce | cg) :: //d
  (ca | cg | cf | ce | cd) :: //e
  (ca | cf | cg | ce) :: Nil //f

  def getPulse(clock: Bool): Bool = {
    val lastClock = RegNext(clock)
    clock && ~lastClock
  }
}

// A frequency divider, used to set the refresh rate for our digit display
class ClockHz(freq: Double) extends Module {
  require(freq > 0 && freq < 100000000) //freq should be slower than 100 Mhz

  val io = IO(new Bundle {
    val clk = Bool(OUTPUT)
  })

  //Find the length of a clock cycle
  val period = (100000000/freq).round //The default clock is 100 Mhz
  val halfPeriod = period/2 //How many default clock cycles before ours toggles

  //Track the default clock's cycles in a counter
  val counterWidth = log2Ceil(halfPeriod)
  val counter = RegInit(0.U(counterWidth.W))
  counter := Mux(counter === (halfPeriod-1).U, 0.U, counter + 1.U)

  //Flip flop when the counter reaches its max
  val flop = RegInit(false.B)
  when(counter === (halfPeriod-1).U) {
    flop := ~flop
    }
  io.clk := flop
}



/* Add this to the system/Configs.scala lowRISC file
class LightDebuggerConfig extends Config(new WithLightDebugger ++ new DefaultConfig)
*/

/* Add this to the subsystem/Configs.scala lowRISC file
class WithLightDebugger extends Config((site, here, up) => {
  case RocketTilesKey => up(RocketTilesKey, site) map { r =>
    r.copy(rocc =
      Seq(
        RoCCParams(
	  opcodes = OpcodeSet.custom0,
	  generator = (p: Parameters) => {
	    val lightDebugger = LazyModule(new LightDebugger()(p))
	    lightDebugger})
      )
    )
  }
})
*/

/* Change the Makefile CONFIG settings to match LightDebuggerConfig
   (in fpga/board/nexys4_ddr) before making the project and bitstream */
