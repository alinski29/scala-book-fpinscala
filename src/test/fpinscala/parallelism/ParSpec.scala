package fpinscala.parallelism

import fpinscala.TestSpec
import fpinscala.parallelism.Par
import fpinscala.parallelism.Par.*

import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.ThreadPoolExecutor

class ParSpec extends TestSpec {

  "Par implementation" should {

    val es = Executors.newCachedThreadPool()

    "compute sum in parallel" in {
      def parSum(ints: IndexedSeq[Int]): Par[Int] =
        // println(s"parSum($ints); thread: ${Thread.currentThread().getName()}")
        if ints.size <= 1 then unit(ints.sum)
        else
          val (l, r) = ints.splitAt(ints.size / 2)
          fork(parSum(l)).map2(fork(parSum(r)))(_ + _)
      parSum(IndexedSeq((1 to 5): _*)).run(es).get shouldEqual 15

    }

    "test map laws" in {
      Par.equal(es)(unit(1).map(_ + 1), unit(2)) shouldBe true
    }

  }

}
