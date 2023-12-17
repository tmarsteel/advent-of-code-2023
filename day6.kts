import java.nio.file.Files
import java.nio.file.Paths
import kotlin.math.ceil
import kotlin.math.floor
import kotlin.math.sqrt

fun getDistanceTraveled(millimetersPerMillisecond: Long, nMillisecondsToMove: Long) = millimetersPerMillisecond * nMillisecondsToMove

fun Double.squared(): Double = this * this

fun Double.isIntegral(): Boolean {
    // stolen from Correto StrictMath.java
    val SIGNF_BIT_MASK = (1L shl (53 - 1)) - 1
    val exponent = StrictMath.getExponent(this)
    if (exponent < 0 || exponent >= 52) {
        return false
    }

    val doppel = java.lang.Double.doubleToRawLongBits(this)
    val mask = SIGNF_BIT_MASK shr exponent

    return mask and doppel == 0L
}

data class Race(val totalMilliseconds: Long, val recordDistanceMillimeters: Long) {
    val numberOfWaysToWinNaive: Long by lazy {
        (0L..totalMilliseconds).asSequence()
            .map { nMillisButtonPressed ->
                getDistanceTraveled(
                    millimetersPerMillisecond = nMillisButtonPressed,
                    nMillisecondsToMove = totalMilliseconds - nMillisButtonPressed
                )
            }
            .filter { it > recordDistanceMillimeters }
            .count()
            .toLong()
    }

    val numberOfWaysToWinSmart: Long by lazy {
        /*
        let t be totalMilliseconds
        let r be recordDistanceMillimeters
        let p be the amount of time we press the button
        */

        val t = totalMilliseconds.toDouble()
        val r = recordDistanceMillimeters.toDouble()

        /*
        we know:

        t > 0   (a 0ms race doesn't make sense)
        p > 0   (we have to hold the button for at least 1ms)
        p < t   (holding the button all race is pointless)
        r > 0   (the record is always greater than 0, as 1 is trivial to achieve)

        and we are looking for all p such that
        r < p * (t - p)

        we can rearrange this to the following (thanks wolfram alpha):

        (t - sqrt(square(t) - 4 * r)) / 2 < p < (sqrt(square(t) - 4 * r) + t) / 2
         */
        val sqrt = sqrt(t.squared() - 4.0 * r)
        val lowerBoundExclusive = (t - sqrt) / 2
        val upperBoundExclusive = (sqrt + t) / 2

        val lowerBoundInclusive = if (lowerBoundExclusive.isIntegral()) lowerBoundExclusive.toLong() + 1 else ceil(lowerBoundExclusive).toLong()
        val upperBoundInclusive = if (upperBoundExclusive.isIntegral()) upperBoundExclusive.toLong() - 1 else floor(upperBoundExclusive).toLong()

        val result = upperBoundInclusive - lowerBoundInclusive + 1
        result
    }
}

fun String.spaceSeparatedIntegers(): List<Long> = trim().split(Regex("\\s+")).map(String::toLong)

fun parseInputPart1(text: String): List<Race> {
    val (timeLine, distanceLine) = text.lines()
    val times = timeLine.removePrefix("Time:").spaceSeparatedIntegers()
    val distances = distanceLine.removePrefix("Distance:").spaceSeparatedIntegers()
    check(times.size == distances.size)
    return times.zip(distances) { time, recordDistance -> Race(time, recordDistance) }
}

fun parseInputPart2(text: String): Race {
    val (timeLine, distanceLine) = text.lines()
    val time = timeLine.removePrefix("Time:").replace(" ", "").toLong()
    val distance = distanceLine.removePrefix("Distance:").replace(" ", "").toLong()

    return Race(time, distance)
}


val races = listOf(parseInputPart2(Files.readString(Paths.get(args[0]))))
val result = races.map { it.numberOfWaysToWinSmart }.reduce(Long::times)

// 227850


println(result)