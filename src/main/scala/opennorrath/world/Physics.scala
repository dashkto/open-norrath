package opennorrath.world

object Physics:
    val DefaultGravity = 80f
    var Gravity = DefaultGravity // GL units/sec² downward acceleration
    def MaxFallSpeed: Float = Gravity * 2f // terminal velocity (scales with gravity)
