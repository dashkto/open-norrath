package opennorrath.network

import scala.compiletime.uninitialized

import java.net.{DatagramPacket, DatagramSocket, InetSocketAddress, SocketTimeoutException}
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicBoolean

enum NetCommand:
  case Connect(host: String, port: Int)
  case SendRaw(data: Array[Byte])
  case Disconnect

/** Background UDP I/O thread for the EQ old protocol.
  *
  * Communicates with the game thread via:
  * - NetCommand queue (game thread → network thread)
  * - PacketHandler.errors queue (network thread → game thread)
  */
class NetworkThread(handler: PacketHandler):
  private val running = AtomicBoolean(false)
  private val commands = ConcurrentLinkedQueue[NetCommand]()
  private var socket: DatagramSocket = uninitialized
  private var address: InetSocketAddress = uninitialized
  private var thread: Thread = uninitialized

  def start(): Unit =
    if running.get() then return
    running.set(true)
    thread = Thread(() => run(), "eq-network")
    thread.setDaemon(true)
    thread.start()

  def stop(): Unit =
    running.set(false)
    if socket != null then
      try socket.close() catch case _: Exception => ()
    if thread != null then thread.join(2000)

  def send(cmd: NetCommand): Unit = commands.add(cmd)

  private def run(): Unit =
    val recvBuf = new Array[Byte](4096)
    while running.get() do
      try
        // Process commands from game thread
        var cmd = commands.poll()
        while cmd != null do
          cmd match
            case NetCommand.Connect(host, port) =>
              if socket != null then
                try socket.close() catch case _: Exception => ()
              address = InetSocketAddress(host, port)
              socket = DatagramSocket()
              socket.setSoTimeout(50) // 50ms for responsive polling
              println(s"[Network] Connecting to $host:$port")

            case NetCommand.SendRaw(data) =>
              if socket != null && address != null && !socket.isClosed then
                val packet = DatagramPacket(data, data.length, address)
                socket.send(packet)
                println(s"[Network] Sent ${data.length}B: ${OldPacket.hexDump(data, Math.min(data.length, 32))}...")

            case NetCommand.Disconnect =>
              if socket != null then
                try socket.close() catch case _: Exception => ()
                socket = null
              println("[Network] Disconnected")
          cmd = commands.poll()

        // Receive packets
        if socket != null && !socket.isClosed then
          try
            val packet = DatagramPacket(recvBuf, recvBuf.length)
            socket.receive(packet)
            val data = new Array[Byte](packet.getLength)
            System.arraycopy(recvBuf, 0, data, 0, packet.getLength)
            println(s"[Network] Recv ${data.length}B: ${OldPacket.hexDump(data, Math.min(data.length, 32))}...")

            OldPacket.decode(data, data.length) match
              case Some(decoded) => handler.handlePacket(decoded)
              case None => println(s"[Network] Failed to decode packet (${data.length}B)")
          catch
            case _: SocketTimeoutException => () // normal

        // Let handler produce outgoing packets (ACKs etc)
        handler.tick()
        var outgoing = handler.pollOutgoing()
        while outgoing.isDefined do
          send(NetCommand.SendRaw(outgoing.get))
          outgoing = handler.pollOutgoing()

      catch
        case e: Exception =>
          if running.get() then
            println(s"[Network] Error: ${e.getClass.getSimpleName}: ${e.getMessage}")
            handler.errors.add(s"Network error: ${e.getMessage}")

    // Cleanup
    println("[Network] Thread exiting")
    if socket != null && !socket.isClosed then
      try socket.close() catch case _: Exception => ()
