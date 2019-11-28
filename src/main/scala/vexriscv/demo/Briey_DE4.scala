package vexriscv.demo

import vexriscv.plugin._
import vexriscv._
import vexriscv.ip.{DataCacheConfig, InstructionCacheConfig}
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba4.axi._
import spinal.lib.com.jtag.Jtag
import spinal.lib.com.spi._
import spinal.lib.com.uart.{Apb3UartCtrl, Uart, UartCtrlGenerics, UartCtrlMemoryMappedConfig}
import spinal.lib.io.TriStateArray
import spinal.lib.memory.sdram._
import spinal.lib.misc.HexTools
import spinal.lib.soc.pinsec.{PinsecTimerCtrl, PinsecTimerCtrlExternal}
import spinal.lib.system.debugger.{JtagAxi4SharedDebugger, JtagBridge, SystemDebugger, SystemDebuggerConfig}

import scala.collection.mutable.ArrayBuffer

case class BrieyDE4Config(axiFrequency : HertzNumber,
                       onChipRamSize : BigInt,
                       cpuPlugins : ArrayBuffer[Plugin[VexRiscv]],
                       uartCtrlConfig : UartCtrlMemoryMappedConfig,
                       spiCtrlConfig : SpiMasterCtrlMemoryMappedConfig)

object BrieyDE4Config{

  def default = {
    val config = BrieyDE4Config(
      axiFrequency = 50 MHz,
      onChipRamSize  = 4 kB,
      uartCtrlConfig = UartCtrlMemoryMappedConfig(
        uartCtrlConfig = UartCtrlGenerics(
          dataWidthMax      = 8,
          clockDividerWidth = 20,
          preSamplingSize   = 1,
          samplingSize      = 5,
          postSamplingSize  = 2
        ),
        txFifoDepth = 16,
        rxFifoDepth = 16
      ),
      spiCtrlConfig =  SpiMasterCtrlMemoryMappedConfig(
        ctrlGenerics = SpiMasterCtrlGenerics(
          ssWidth = 1,
          timerWidth = 8,
          dataWidth = 8
        ),
        cmdFifoDepth = 32,
        rspFifoDepth = 32
      ),
      cpuPlugins = ArrayBuffer(
        new PcManagerSimplePlugin(0x40000000l, false),
        //          new IBusSimplePlugin(
        //            interfaceKeepData = false,
        //            catchAccessFault = true
        //          ),
        new IBusCachedPlugin(
          resetVector = 0x40000000l,
          prediction = STATIC,
          config = InstructionCacheConfig(
            cacheSize = 4096,
            bytePerLine =32,
            wayCount = 1,
            addressWidth = 32,
            cpuDataWidth = 32,
            memDataWidth = 32,
            catchIllegalAccess = true,
            catchAccessFault = true,
            asyncTagMemory = false,
            twoCycleRam = true,
            twoCycleCache = true
          )
          //            askMemoryTranslation = true,
          //            memoryTranslatorPortConfig = MemoryTranslatorPortConfig(
          //              portTlbSize = 4
          //            )
        ),
        //                    new DBusSimplePlugin(
        //                      catchAddressMisaligned = true,
        //                      catchAccessFault = true
        //                    ),
        new DBusCachedPlugin(
          config = new DataCacheConfig(
            cacheSize         = 4096,
            bytePerLine       = 32,
            wayCount          = 1,
            addressWidth      = 32,
            cpuDataWidth      = 32,
            memDataWidth      = 32,
            catchAccessError  = true,
            catchIllegal      = true,
            catchUnaligned    = true
          ),
          memoryTranslatorPortConfig = null
          //            memoryTranslatorPortConfig = MemoryTranslatorPortConfig(
          //              portTlbSize = 6
          //            )
        ),
        new StaticMemoryTranslatorPlugin(
          ioRange      = _(31 downto 28) === 0xF
        ),
        new DecoderSimplePlugin(
          catchIllegalInstruction = true
        ),
        new RegFilePlugin(
          regFileReadyKind = plugin.SYNC,
          zeroBoot = false
        ),
        new IntAluPlugin,
        new SrcPlugin(
          separatedAddSub = false,
          executeInsertion = true
        ),
        new FullBarrelShifterPlugin,
        new MulPlugin,
        new DivPlugin,
        new HazardSimplePlugin(
          bypassExecute           = true,
          bypassMemory            = true,
          bypassWriteBack         = true,
          bypassWriteBackBuffer   = true,
          pessimisticUseSrc       = false,
          pessimisticWriteRegFile = false,
          pessimisticAddressMatch = false
        ),
        new BranchPlugin(
          earlyBranch = false,
          catchAddressMisaligned = true
        ),
        new CsrPlugin(
          config = CsrPluginConfig(
            catchIllegalAccess = false,
            mvendorid      = null,
            marchid        = null,
            mimpid         = null,
            mhartid        = null,
            misaExtensionsInit = 66,
            misaAccess     = CsrAccess.NONE,
            mtvecAccess    = CsrAccess.NONE,
            mtvecInit      = 0x40000000l,
            mepcAccess     = CsrAccess.READ_WRITE,
            mscratchGen    = false,
            mcauseAccess   = CsrAccess.READ_ONLY,
            mbadaddrAccess = CsrAccess.READ_ONLY,
            mcycleAccess   = CsrAccess.NONE,
            minstretAccess = CsrAccess.NONE,
            ecallGen       = false,
            wfiGenAsWait         = false,
            ucycleAccess   = CsrAccess.NONE
          )
        ),
        new YamlPlugin("cpu0.yaml")
      )
    )
    config
  }
}

// The OnChipROM that will contain the contents of the first program
class OnChipROM extends BlackBox{
  val io = new Bundle{
    val iClk = in Bool
    val iCS = in Bool
    val iWrEn = in Bool
    val iAddr = in UInt(11 bits)
    val iWrByteEn = in Bits(4 bits)
    val iWrData = in Bits(32 bits)
    val oRdData = out Bits(32 bits)
  }
  noIoPrefix() // remove io_ prefix
  mapClockDomain(clock=io.iClk) // map to the current clockDomain
}

// The QSYS blackbox utilities (translated from the keystone-hardware)
class QuartusDDR extends Bundle {
  val memory_mem_a                = out (UInt((14) bits))
  val memory_mem_ba               = out (UInt((3) bits))
  val memory_mem_ck               = out (UInt((2) bits))
  val memory_mem_ck_n             = out (UInt((2) bits))
  val memory_mem_cke              = out (UInt((2) bits))
  val memory_mem_cs_n             = out (UInt((2) bits))
  val memory_mem_dm               = out (UInt((8) bits))
  val memory_mem_ras_n            = out (Bool())
  val memory_mem_cas_n            = out (Bool())
  val memory_mem_we_n             = out (Bool())
  val memory_mem_dq               = out (Bits(64 bits)) // WARNING: inout
  val memory_mem_dqs              = out (Bits(8 bits)) // WARNING: inout
  val memory_mem_dqs_n            = out (Bits(8 bits)) // WARNING: inout
  val memory_mem_odt              = out (UInt((2) bits))
}

trait QuartusClocksReset extends Bundle {
  //inputs
  //"NO_BUFFER" clock source (must be connected to IBUF outside of IP)
  val refclk_clk            = in (Bool())
  val reset_reset_n         = in (Bool())
  val dimmclk_clk           = out (Bool())
}

trait QuartusUserSignals extends Bundle {
  val oct_rdn               = in (Bool())
  val oct_rup               = in (Bool())
  val mem_if_ddr2_emif_0_status_local_init_done = out (Bool())
  val mem_if_ddr2_emif_0_status_local_cal_success = out (Bool())
  val mem_if_ddr2_emif_0_status_local_cal_fail = out (Bool())
}

class QuartusIO extends QuartusDDR with QuartusUserSignals

class qsys extends BlackBox {
  val io = new QuartusIO with QuartusClocksReset {
    //axi_s
    //slave interface write address ports
    val axi4_awid = in (UInt((4) bits))
    val axi4_awaddr = in (UInt((32) bits))
    val axi4_awlen = in (UInt((8) bits))
    val axi4_awsize = in (UInt((3) bits))
    val axi4_awburst = in (Bits((2) bits))
    val axi4_awvalid = in (Bool())
    val axi4_awready = out (Bool())
    //slave interface write data ports
    val axi4_wdata = in (Bits((32) bits))
    val axi4_wstrb = in (Bits((4) bits))
    val axi4_wlast = in (Bool())
    val axi4_wvalid = in (Bool())
    val axi4_wready = out (Bool())
    //slave interface write response ports
    val axi4_bready = in (Bool())
    val axi4_bid = out (UInt((4) bits))
    val axi4_bresp = out (Bits((2) bits))
    val axi4_bvalid = out (Bool())
    //slave interface read address ports
    val axi4_arid = in (UInt((4) bits))
    val axi4_araddr = in (UInt((32) bits))
    val axi4_arlen = in (UInt((8) bits))
    val axi4_arsize = in (UInt((3) bits))
    val axi4_arburst = in (Bits((2) bits))
    val axi4_arvalid = in (Bool())
    val axi4_arready = out (Bool())
    //slave interface read data ports
    val axi4_rready = in (Bool())
    val axi4_rid = out (UInt((4) bits))
    val axi4_rdata = out (Bits((32) bits))
    val axi4_rresp = out (Bits((2) bits))
    val axi4_rlast = out (Bool())
    val axi4_rvalid = out (Bool())
  }
  noIoPrefix() // remove io_ prefix
}

class Axi4ToQSYS(axiConfig: Axi4Config, offset: BigInt) extends Component {
  val io = new Bundle {
    val axi = slave(Axi4(axiConfig))
    val port = new QuartusIO
    val ckrst = new Bundle with QuartusClocksReset
  }
  
  val capacity: BigInt = 0x40000000l
  val axi_async = io.axi
  val blackbox = new qsys

  //inouts
  io.port.memory_mem_dq := blackbox.io.memory_mem_dq
  io.port.memory_mem_dqs_n := blackbox.io.memory_mem_dqs_n
  io.port.memory_mem_dqs := blackbox.io.memory_mem_dqs

  //outputs
  io.port.memory_mem_a            := blackbox.io.memory_mem_a
  io.port.memory_mem_ba           := blackbox.io.memory_mem_ba
  io.port.memory_mem_ras_n        := blackbox.io.memory_mem_ras_n
  io.port.memory_mem_cas_n        := blackbox.io.memory_mem_cas_n
  io.port.memory_mem_we_n         := blackbox.io.memory_mem_we_n
  io.port.memory_mem_ck           := blackbox.io.memory_mem_ck
  io.port.memory_mem_ck_n         := blackbox.io.memory_mem_ck_n
  io.port.memory_mem_cke          := blackbox.io.memory_mem_cke
  io.port.memory_mem_cs_n         := blackbox.io.memory_mem_cs_n
  io.port.memory_mem_dm           := blackbox.io.memory_mem_dm
  io.port.memory_mem_odt          := blackbox.io.memory_mem_odt

  //inputs
  //NO_BUFFER clock
  blackbox.io.refclk_clk       := io.ckrst.refclk_clk
  blackbox.io.reset_reset_n := io.ckrst.reset_reset_n
  io.ckrst.dimmclk_clk       := blackbox.io.dimmclk_clk
  blackbox.io.oct_rdn       := io.port.oct_rdn
  blackbox.io.oct_rup       := io.port.oct_rup
  io.port.mem_if_ddr2_emif_0_status_local_init_done := blackbox.io.mem_if_ddr2_emif_0_status_local_init_done
  io.port.mem_if_ddr2_emif_0_status_local_cal_success := blackbox.io.mem_if_ddr2_emif_0_status_local_cal_success
  io.port.mem_if_ddr2_emif_0_status_local_cal_fail := blackbox.io.mem_if_ddr2_emif_0_status_local_cal_fail

  val awaddr = axi_async.aw.addr - offset
  val araddr = axi_async.ar.addr - offset

  //slave AXI interface write address ports
  blackbox.io.axi4_awid    := axi_async.aw.id
  blackbox.io.axi4_awaddr  := awaddr //truncated
  blackbox.io.axi4_awlen   := axi_async.aw.len
  blackbox.io.axi4_awsize  := axi_async.aw.size
  blackbox.io.axi4_awburst := axi_async.aw.burst
  blackbox.io.axi4_awvalid := axi_async.aw.valid
  axi_async.aw.ready        := blackbox.io.axi4_awready

  //slave interface write data ports
  blackbox.io.axi4_wdata   := axi_async.w.data
  blackbox.io.axi4_wstrb   := axi_async.w.strb
  blackbox.io.axi4_wlast   := axi_async.w.last
  blackbox.io.axi4_wvalid  := axi_async.w.valid
  axi_async.w.ready         := blackbox.io.axi4_wready

  //slave interface write response
  blackbox.io.axi4_bready  := axi_async.b.ready
  axi_async.b.id       := blackbox.io.axi4_bid
  axi_async.b.resp     := blackbox.io.axi4_bresp
  axi_async.b.valid         := blackbox.io.axi4_bvalid

  //slave AXI interface read address ports
  blackbox.io.axi4_arid    := axi_async.ar.id
  blackbox.io.axi4_araddr  := araddr // truncated
  blackbox.io.axi4_arlen   := axi_async.ar.len
  blackbox.io.axi4_arsize  := axi_async.ar.size
  blackbox.io.axi4_arburst := axi_async.ar.burst
  blackbox.io.axi4_arvalid := axi_async.ar.valid
  axi_async.ar.ready        := blackbox.io.axi4_arready

  //slace AXI interface read data ports
  blackbox.io.axi4_rready  := axi_async.r.ready
  axi_async.r.id       := blackbox.io.axi4_rid
  axi_async.r.data     := blackbox.io.axi4_rdata
  axi_async.r.resp     := blackbox.io.axi4_rresp
  axi_async.r.last     := blackbox.io.axi4_rlast
  axi_async.r.valid         := blackbox.io.axi4_rvalid
}

class BrieyDE4(config: BrieyDE4Config) extends Component{

  //Legacy constructor
  def this(axiFrequency: HertzNumber) {
    this(BrieyDE4Config.default.copy(axiFrequency = axiFrequency))
  }

  import config._
  val debug = true
  val interruptCount = 4

  val io = new Bundle{
    //Clocks / reset
    val asyncReset = in Bool
    val ddr_reset_n = in Bool
    val refClk     = in Bool

    //Main components IO
    val jtag       = slave(Jtag())
    val port       = new QuartusIO

    //Peripherals IO
    val gpioA         = master(TriStateArray(16 bits))
    val uart          = master(Uart())
    val spi           = master(SpiMaster(ssWidth = spiCtrlConfig.ctrlGenerics.ssWidth))
    val timerExternal = in(PinsecTimerCtrlExternal())
    val coreInterrupt = in Bool
  }

  // The system (AXI) clock
  val axiClk     = Bool

  val resetCtrlClockDomain = ClockDomain(
    clock = axiClk,
    config = ClockDomainConfig(
      resetKind = BOOT
    )
  )

  val resetCtrl = new ClockingArea(resetCtrlClockDomain) {
    val systemResetUnbuffered  = False
//    val coreResetUnbuffered = False

    //Implement an counter to keep the reset axiResetOrder high 64 cycles
    // Also this counter will automaticly do a reset when the system boot.
    val systemResetCounter = Reg(UInt(6 bits)) init(0)
    when(systemResetCounter =/= U(systemResetCounter.range -> true)){
      systemResetCounter := systemResetCounter + 1
      systemResetUnbuffered := True
    }
    when(BufferCC(io.asyncReset)){
      systemResetCounter := 0
    }

    //Create all reset used later in the design
    val systemReset  = RegNext(systemResetUnbuffered)
    val axiReset     = RegNext(systemResetUnbuffered)
  }

  val axiClockDomain = ClockDomain(
    clock = axiClk,
    reset = resetCtrl.axiReset,
    frequency = FixedFrequency(axiFrequency) //The frequency information is used by the SDRAM controller
  )

  val debugClockDomain = ClockDomain(
    clock = axiClk,
    reset = resetCtrl.systemReset,
    frequency = FixedFrequency(axiFrequency)
  )

  val axi = new ClockingArea(axiClockDomain) {
    val ram = new Axi4SharedToBram(
      addressAxiWidth = 32,
      addressBRAMWidth = 11,
      dataWidth = 32,
      idWidth = 4
    )
    val rom = new OnChipROM
    rom.io.iAddr := ram.io.bram.addr
    //rom.io.iClk := axiClk // NOTE: Seems that if the blackbox already defines the domain, there is no need to connect this
    rom.io.iCS := ram.io.bram.en
    rom.io.iWrByteEn := ram.io.bram.we
    rom.io.iWrData := ram.io.bram.wrdata
    rom.io.iWrEn := ram.io.bram.we =/= 0 // This is also ok?
    ram.io.bram.rddata := rom.io.oRdData

    val ddr = new Axi4ToQSYS(
      Axi4Config(
        addressWidth = 32,
        dataWidth    = 32,
        idWidth      = 4,
        useLock      = false,
        useRegion    = false,
        useCache     = false,
        useProt      = false,
        useQos       = false
      ),
      0x80000000l
    )
    axiClk := ddr.io.ckrst.dimmclk_clk
    ddr.io.ckrst.refclk_clk := io.refClk
    ddr.io.ckrst.reset_reset_n := io.ddr_reset_n
    io.port <> ddr.io.port

    val apbBridge = Axi4SharedToApb3Bridge(
      addressWidth = 20,
      dataWidth    = 32,
      idWidth      = 4
    )

    val gpioACtrl = Apb3Gpio(
      gpioWidth = 16,
      withReadSync = true
    )
    val timerCtrl = PinsecTimerCtrl()
    val uartCtrl = Apb3UartCtrl(uartCtrlConfig)
    val spiCtrl = Apb3SpiMasterCtrl(spiCtrlConfig)

    val core = new Area{
      val config = VexRiscvConfig(
        plugins = cpuPlugins += new DebugPlugin(debugClockDomain)
      )

      val cpu = new VexRiscv(config)
      var iBus : Axi4ReadOnly = null
      var dBus : Axi4Shared = null
      for(plugin <- config.plugins) plugin match{
        case plugin : IBusSimplePlugin => iBus = plugin.iBus.toAxi4ReadOnly()
        case plugin : IBusCachedPlugin => iBus = plugin.iBus.toAxi4ReadOnly()
        case plugin : DBusSimplePlugin => dBus = plugin.dBus.toAxi4Shared()
        case plugin : DBusCachedPlugin => dBus = plugin.dBus.toAxi4Shared(true)
        case plugin : CsrPlugin        => {
          plugin.externalInterrupt := BufferCC(io.coreInterrupt)
          plugin.timerInterrupt := timerCtrl.io.interrupt
        }
        case plugin : DebugPlugin      => debugClockDomain{
          resetCtrl.axiReset setWhen(RegNext(plugin.io.resetOut))
          io.jtag <> plugin.io.bus.fromJtag()
        }
        case _ =>
      }
    }


    val axiCrossbar = Axi4CrossbarFactory()

    axiCrossbar.addSlaves(
      ram.io.axi       -> (0x40000000L,   onChipRamSize),
      ddr.io.axi       -> (0x80000000L,   ddr.capacity),
      apbBridge.io.axi -> (0xF0000000L,   1 MB)
    )

    axiCrossbar.addConnections(
      core.iBus       -> List(ram.io.axi, ddr.io.axi),
      core.dBus       -> List(ram.io.axi, ddr.io.axi, apbBridge.io.axi)
    )


    axiCrossbar.addPipelining(apbBridge.io.axi)((crossbar,bridge) => {
      crossbar.sharedCmd.halfPipe() >> bridge.sharedCmd
      crossbar.writeData.halfPipe() >> bridge.writeData
      crossbar.writeRsp             << bridge.writeRsp
      crossbar.readRsp              << bridge.readRsp
    })

    // TODO: IMPORTANT: Axi4 is not compatible with the addPipelining. Probably is not necessary, but who knows?
    /*axiCrossbar.addPipelining(ddr.io.axi)((crossbar,ctrl) => {
      crossbar.sharedCmd.halfPipe()  >>  ctrl.sharedCmd
      crossbar.writeData            >/-> ctrl.writeData
      crossbar.writeRsp              <<  ctrl.writeRsp
      crossbar.readRsp               <<  ctrl.readRsp
    })*/

    axiCrossbar.addPipelining(ram.io.axi)((crossbar,ctrl) => {
      crossbar.sharedCmd.halfPipe()  >>  ctrl.sharedCmd
      crossbar.writeData            >/-> ctrl.writeData
      crossbar.writeRsp              <<  ctrl.writeRsp
      crossbar.readRsp               <<  ctrl.readRsp
    })

    axiCrossbar.addPipelining(core.dBus)((cpu,crossbar) => {
      cpu.sharedCmd             >>  crossbar.sharedCmd
      cpu.writeData             >>  crossbar.writeData
      cpu.writeRsp              <<  crossbar.writeRsp
      cpu.readRsp               <-< crossbar.readRsp //Data cache directly use read responses without buffering, so pipeline it for FMax
    })

    axiCrossbar.build()

    val apbDecoder = Apb3Decoder(
      master = apbBridge.io.apb,
      slaves = List(
        gpioACtrl.io.apb -> (0x00000, 4 kB),
        spiCtrl.io.apb   -> (0x10000, 4 kB),
        uartCtrl.io.apb  -> (0x20000, 4 kB),
        timerCtrl.io.apb -> (0x30000, 4 kB)
      )
    )
  }

  io.gpioA          <> axi.gpioACtrl.io.gpio
  io.timerExternal  <> axi.timerCtrl.io.external
  io.uart           <> axi.uartCtrl.io.uart
  io.spi            <> axi.spiCtrl.io.spi
}

//DE4-SoC
object BrieyDE4{
  def main(args: Array[String]) {
    val config = SpinalConfig()
    config.generateVerilog({
      val toplevel = new BrieyDE4(BrieyDE4Config.default)
      toplevel
    })
  }
}
