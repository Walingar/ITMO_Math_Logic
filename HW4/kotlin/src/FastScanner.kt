import java.io.*
import java.util.*

class FastScanner(f: File) {
    private lateinit var br: BufferedReader
    private var st: StringTokenizer? = null

    init {
        try {
            br = BufferedReader(FileReader(f))
        } catch (e: FileNotFoundException) {
            e.printStackTrace()
        }

    }

    operator fun next(): String {
        while (st == null || !st!!.hasMoreTokens()) {
            try {
                st = StringTokenizer(br.readLine())
            } catch (e: IOException) {
                e.printStackTrace()
            }

        }
        return st!!.nextToken()
    }

    fun nextLine(): String {
        return br.readLine()
    }

    fun nextInt(): Int {
        return next().toInt()
    }

    fun nextDouble(): Double {
        return next().toDouble()
    }
}