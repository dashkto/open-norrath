package opennorrath.network

/** Common interface for network thread implementations.
  * Both Mac (NetworkThread) and Titanium (TitaniumNetworkThread) provide
  * the same start/stop/send lifecycle, differing only in wire protocol.
  */
trait EqNetworkThread:
  def start(): Unit
  def stop(): Unit
  def send(cmd: NetCommand): Unit
