import java.io.*

const val fileNameInput = "input.txt"
const val fileNameOutput = "output.txt"
const val MAX_N = 100 + 5

val edges = Array(MAX_N, { ArrayList<Edge>() })
val greater = Array(MAX_N, { HashSet<Int>() })
val less = Array(MAX_N, { HashSet<Int>() })
val was = BooleanArray(MAX_N, { false })
val sum = Array(MAX_N, { IntArray(MAX_N, { -1 }) })
val mul = Array(MAX_N, { IntArray(MAX_N, { -1 }) })
val impl = Array(MAX_N, { IntArray(MAX_N, { -1 }) })
var min_elem = MAX_N
var inp = FastScanner(File(fileNameInput))
var out = File(fileNameOutput).printWriter()
var n = 0


fun dfs(v: Int, start: Int) {
    was[v] = true
    greater[start].add(v)
    less[v].add(start)
    for (edge in edges[v]) {
        if (!was[edge.to]) {
            dfs(edge.to, start)
        }
    }
}

fun aMinb(a: Int, b: Int): Boolean {
    if (a in less[b]) {
        return true
    }
    return false
}

fun aBigb(a: Int, b: Int): Boolean {
    if (a in greater[b]) {
        return true
    }
    return false
}

fun aUndb(a: Int, b: Int): Boolean {
    if (a in greater[b]) {
        return false
    } else if (a in less[b]) {
        return false
    }
    return true
}


fun checkSum() {
    for (a in 1..n) {
        for (b in 1..n) {
            var ans = -1
            val temp = ArrayList<Int>()
            for (c in greater[b]) {
                if (c in greater[a]) {
                    if (ans == -1 || aMinb(c, ans)) {
                        ans = c
                    }
                    if (aUndb(c, ans)) {
                        temp.add(c)
                    }
                }
            }
            if (ans == -1) {
                out.print(
                        "Операция '+' не определена: $a+$b"
                )
                out.close()
                System.exit(0)
            }
            for (v in temp) {
                if (aUndb(v, ans)) {
                    out.print(
                            "Операция '+' не определена: $a+$b"
                    )
                    out.close()
                    System.exit(0)
                }
            }
            sum[a][b] = ans
        }
    }
}

fun checkMul() {
    for (a in 1..n) {
        for (b in 1..n) {
            var ans = -1
            val temp = ArrayList<Int>()
            for (c in less[b]) {
                if (c in less[a]) {
                    if (ans == -1 || aBigb(c, ans)) {
                        ans = c
                    }
                    if (aUndb(c, ans)) {
                        temp.add(c)
                    }
                }
            }
            if (ans == -1) {
                out.print(
                        "Операция '*' не определена: $a*$b"
                )
                out.close()
                System.exit(0)
            }
            for (v in temp) {
                if (aUndb(v, ans)) {
                    out.print(
                            "Операция '*' не определена: $a*$b"
                    )
                    out.close()
                    System.exit(0)
                }
            }
            mul[a][b] = ans
        }
    }
}

fun checkDist() {
    for (a in 1..n) {
        for (b in 1..n) {
            var ans = -1
            for (c in 1..n) {
                if (mul[a][sum[b][c]] != sum[mul[a][b]][mul[a][c]]) {
                    out.print(
                            "Нарушается дистрибутивность: $a*($b+$c)"
                    )
                    out.close()
                    System.exit(0)
                }
                if (aMinb(mul[c][a], b)) {
                    if (ans == -1 || aBigb(c, ans)) {
                        ans = c
                    }
                }
            }
            impl[a][b] = ans
        }
    }
}

fun findMin() {
    for (a in 1..n) {
        var flag = true
        for (b in 1..n) {
            if (a != b) {
                if (!aMinb(a, b)) {
                    flag = false
                    break
                }
            }
        }
        if (flag) {
            min_elem = a
            break
        }
    }
}

fun checkBool() {
    for (a in 1..n) {
        if (sum[a][impl[a][min_elem]] != impl[a][a]) {
            out.print(
                    "Не булева алгебра: $a+~$a"
            )
            out.close()
            System.exit(0)
        }
    }
}

fun main(args: Array<String>) {
    n = inp.nextInt()
    for (from in 1..n) {
        val vertex = inp.nextLine().trim().split(' ').map { it.toInt() }
        for (to in vertex) {
            edges[from].add(Edge(from, to))
        }
    }
    for (v in 1..n) {
        was.fill(false)
        dfs(v, v)
    }
    checkSum()
    checkMul()
    checkDist()
    findMin()
    checkBool()
    out.print(
            "Булева алгебра"
    )
    out.close()
}
