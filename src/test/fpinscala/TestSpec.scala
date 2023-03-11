//> using dep "org.scalatest::scalatest:3.2.15"
package fpinscala

import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers._
import org.scalatest.wordspec.AnyWordSpecLike

trait TestSpec extends AnyWordSpecLike with should.Matchers
