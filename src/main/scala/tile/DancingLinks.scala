// Accelerator for Dancing Links manipulations
// (c) Maddie Burbage, August 2020, for the Bailey Research Group at Williams

package freechips.rocketchip.tile

import Chisel._
import freechips.rocketchip.config._ //For Config
import freechips.rocketchip.diplomacy._ //For LazyModule
import freechips.rocketchip.rocket.{HellaCacheReq} //For cache accesses
import chisel3.VecInit

// Wrapper for the accelerator
class DancingLinks()(implicit p: Parameters) extends LazyRoCC {
  override lazy val module = new DancingLinksImp(this)
}

// Main accelerator class
class DancingLinksImp(outer: DancingLinks)(implicit p: Parameters) extends LazyRoCCModule(outer) {

  //State control
  val faulted = io.mem.s2_xcpt.asUInt.orR
  val memState = DancingLinksImp.memFSM(io.mem.req.fire(), io.mem.resp.valid, faulted)
  val doneWithMem = ((io.mem.resp.valid || faulted) && (memState === DancingLinksImp.m_finish1 || memState === DancingLinksImp.m_fix)) || (faulted && memState === DancingLinksImp.m_wait)
  val dancingFired = Wire(Bool())
  val setupFired = Wire(Bool())
  
  val state = DancingLinksImp.FSM(dancingFired, setupFired || doneWithMem, io.resp.fire())

  // Instruction data
  val setupInst = io.cmd.bits.inst.funct(1) //Setup or Dancing function
  dancingFired := io.cmd.fire() && !setupInst
  setupFired := io.cmd.fire() && setupInst
  val cover = Reg(init = Bool(true)) // Whether to cover or uncover the element
  val rd = Reg(init = io.cmd.bits.inst.rd)
  val exceptions = Reg(init=0.U(6.W))
  val fault_code = io.mem.s2_xcpt.asUInt | exceptions
  exceptions := fault_code

  //RoCC Interface
  val structWidth = Reg(init = 16.U(64.W)) //Bytes of width for the element struct
  io.interrupt := Bool(false)
  io.cmd.ready := state === DancingLinksImp.s_idle && memState === DancingLinksImp.m_load
  io.busy := state =/= DancingLinksImp.s_idle //change to not idle when you can!!!
  io.resp.valid := state === DancingLinksImp.s_resp
  io.resp.bits.data := fault_code //Use non-zero response to signify a page fault error
  io.resp.bits.rd := rd

  // Setup data
  val nameArray = Reg(init = 0.U(64.W)) //Index to base of the array of structs
  val elementIndex = Reg(init = io.cmd.bits.rs1) //Index to current struct
  
  // Instruction arrives
  when(io.cmd.fire()) {
    rd := io.cmd.bits.inst.rd
    exceptions := 0.U
    when(setupInst) { //Setting up the accelerator
      nameArray := io.cmd.bits.rs1
      structWidth := io.cmd.bits.rs2
    } .otherwise { //Covering or uncovering an element
      elementIndex := io.cmd.bits.rs1
      cover := (io.cmd.bits.inst.funct(0) === 0.U) //Function bit 0 controls uncovering/covering
    }
  }

  // Base address for the elements
  val offsetAddress = nameArray + 0.U //Base address, aligned to skip nothing

  // Read address:
  val elementAddress = offsetAddress + elementIndex * structWidth

  // Neighbor elements' addresses
  val firstNeighborIndex = Reg(init = 0.U(32.W))
  val secondNeighborIndex = Reg(init = 0.U(32.W))
  val firstNeighborAddress = offsetAddress + firstNeighborIndex * structWidth
  val secondNeighborAddress = offsetAddress + secondNeighborIndex * structWidth

  // Write addresses:
  val firstNeighborChange = firstNeighborAddress + 4.U //Change 2nd index
  val secondNeighborChange = secondNeighborAddress + 0.U //Change 1st index

  // mem interface setup
  val reqState = (memState === DancingLinksImp.m_load) || (memState === DancingLinksImp.m_storeFirst) || (memState === DancingLinksImp.m_storeSecond)
  io.mem.req.valid := state === DancingLinksImp.s_busy && reqState
  io.mem.req.bits.phys := Bool(false)

  when(memState === DancingLinksImp.m_load) { //Read from the element
    io.mem.req.bits.cmd := 0.U //Load operation
    io.mem.req.bits.addr := elementAddress
    io.mem.req.bits.tag := elementAddress(5,0)
    io.mem.req.bits.typ := 3.U //For 64-bit width of response (read 2 ints)

  } .elsewhen(memState === DancingLinksImp.m_wait) {
    when(io.mem.resp.valid) {
      firstNeighborIndex := (io.mem.resp.bits.data)(31, 0)
      secondNeighborIndex := (io.mem.resp.bits.data)(63, 32)
    }
  } .otherwise { //Write to a neighbor's fields
    io.mem.req.bits.cmd := 1.U //Store operation
    io.mem.req.bits.typ := 2.U //For 32-bit width of write (write 1 int)

    when(memState === DancingLinksImp.m_storeFirst) { //Change first neighbor
      io.mem.req.bits.addr := firstNeighborChange
      io.mem.req.bits.data := Mux(cover, secondNeighborIndex, elementIndex) << 32
      io.mem.req.bits.tag := Cat(0.U, firstNeighborChange(4,0))
    } .elsewhen(memState === DancingLinksImp.m_storeSecond) { //Change second neighbor
      io.mem.req.bits.addr := secondNeighborChange
      io.mem.req.bits.data := Mux(cover, firstNeighborIndex, elementIndex)
      io.mem.req.bits.tag := Cat(1.U, secondNeighborChange(4,0))
    }
  }
}

object DancingLinksImp {
  val s_idle :: s_busy :: s_resp :: Nil = Enum(Bits(), 3)
  val m_load :: m_wait :: m_storeFirst :: m_storeSecond :: m_finish1 :: m_finish2 :: m_fix :: Nil = Enum(Bits(), 7)

  def FSM(moveLinks: Bool, finishedTask: Bool, sentResp: Bool): UInt = {
    // Accelerator states
    val state = Reg(init = s_idle) //Idle until handling an instruction

    //State transitions
    when(moveLinks) {
      state := s_busy
      printf("State now busy\n")
    }

    when(finishedTask) {
      state := s_resp
      printf("State now responding\n")
    }

    when(sentResp) {
      state := s_idle
      printf("State now idle\n")
    }

    state
  }

  def memFSM(sentReq: Bool, gotResp: Bool, faulted: Bool): UInt = {
    // Memory states
    val mem_state = Reg(init = m_load)
    val storesLeft = Reg(init = 0.U(2.W))
    val currentStoresLeft = storesLeft + sentReq - gotResp - faulted
    storesLeft := currentStoresLeft

    when(mem_state === m_load && sentReq) {
      mem_state := m_wait
      printf("Mem state waiting for a load\n")
    }
    when(mem_state === m_wait && gotResp) {
      mem_state := m_storeFirst
      printf("Mem state storing first\n")
    }
    when(mem_state === m_storeFirst && sentReq) {
      mem_state := m_storeSecond
      printf("Mem state storing second\n")
    }
    when(mem_state === m_storeSecond && sentReq) {
      when(currentStoresLeft === 1.U) {
        mem_state := m_finish1
      } .otherwise {
        mem_state := m_finish2
      }
      printf("Mem state finishing stores\n")
    }
    when(mem_state === m_finish2 && gotResp) {
      mem_state := m_finish1
      printf("Mem state finishing store\n")
    }
    when(mem_state === m_finish1 && gotResp) {
      mem_state := m_load
      printf("Mem state loading\n")
    }

    when(mem_state =/= m_fix && faulted) {
      when(mem_state === m_wait) {
        mem_state := m_load
      } .otherwise {
        mem_state := m_fix
      }
      printf("Mem failed\n")
    }
    when(mem_state === m_fix && currentStoresLeft === 0.U) {
      mem_state := m_load
      printf("Mem state waiting for a load\n")
    }

    mem_state
  }
}