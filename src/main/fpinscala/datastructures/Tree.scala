package fpinscala.datastructures

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + l.size + r.size

  /* Exercise 3.26
    Write a function depth that returns the maximum path length from the root of
    a tree to any leaf.
   */
  def depth: Int = this match
    case Leaf(v)      => 0
    case Branch(l, r) => 1 + (l.depth.max(r.depth))

  /* Exercise 3.27
    Write a function map, analogous to the method of the same name on List,
    that modifies each element in a tree with a given function.
   */
  def map[B](f: A => B): Tree[B] = this match
    case Leaf(v)      => Leaf(f(v))
    case Branch(l, r) => Branch(l.map(f), r.map(f))

  /* Exercis3 3.28
  Generalize size, maximum, depth, and map, writing a new function fold that abstracts over their similarities
   */
  def fold[B](f: A => B, g: (B, B) => B): B = this match
    case Leaf(v)      => f(v)
    case Branch(l, r) => g(l.fold(f, g), r.fold(f, g))

  def sizeViaFold: Int =
    fold(x => 1, (a, b) => 1 + a + b)

  def depthViaFold: Int =
    fold(a => 0, (d1, d2) => 1 + (d1 max d2))

  def mapViaFold[B](f: A => B): Tree[B] =
    fold(a => Leaf(f(a)), Branch(_, _))

object Tree:
  extension (t: Tree[Int])
    def firstPositive: Int = t match
      case Leaf(i) => i
      case Branch(l, r) =>
        val lpos = l.firstPositive
        if lpos > 0 then lpos else r.firstPositive

  /* Exercise 3.25
    Write a function maximum that returns the maximum element in a Tree[Int].
    (Note: In Scala, you can use x.max(y) to compute the maximum of two integers x and y)
   */
  extension (t: Tree[Int])
    def maximum: Int = t match
      case Leaf(v)      => v
      case Branch(l, r) => l.maximum max r.maximum

    def maximumViaFold =
      t.fold(a => a, _ max _)
