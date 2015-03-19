/* Comments Sample
 * ===============
 * This file shows how markdown can be used inside comments to add **important** background
 * information to your source code.
 *
 * - Apples
 * - Bananas
 * 
 * Some Text inbetween
 *
 * > This is really
 * > awesome!
 *
 * "Foobar"
 *
 *     val foo = 42
 *
 * Also tables are supported
 *
 * | First Header | Second Header |         Third Header |  
 * | :----------- | :-----------: | -------------------: |  
 * | First row    |      Data     | Very long data entry |  
 * | Second row   |    **Cell**   |               *Cell* | 
 *
 * Definition Lists are also supported by pegdown...
 *
 * Apple
 * :   Pomaceous fruit of plants of the genus Malus in 
 *    the family Rosaceae.
 *
 * Orange
 * :   The fruit of an evergreen tree of the genus Citrus.
 *
 */

/** An implementation of the `Buffer` class using an array to
 *  represent the assembled sequence internally. Append, update and random
 *  access take constant time (amortized time). Prepends and removes are
 *  linear in the buffer size.
 *
 *
 *  {{{
 *    var foo = 42;
 *    class Baz extends { var bar = 'fsdfds; var baz = "fsdf" }
 *  }}}
 *
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   1
 *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-mutable-collection-classes.html#array_buffers "Scala's Collection Library overview"]]
 *  section on `Array Buffers` for more information.

 *
 *  @tparam A    the type of this arraybuffer's elements.
 *
 *  @define Coll `ArrayBuffer`
 *  @define coll arraybuffer
 *  @define thatinfo the class of the returned collection. In the standard library configuration,
 *    `That` is always `ArrayBuffer[B]` because an implicit of type `CanBuildFrom[ArrayBuffer, B, ArrayBuffer[B]]`
 *    is defined in object `ArrayBuffer`.
 *  @define bfinfo an implicit value of class `CanBuildFrom` which determines the
 *    result class `That` from the current representation type `Repr`
 *    and the new element type `B`. This is usually the `canBuildFrom` value
 *    defined in object `ArrayBuffer`.
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
trait Comments { self: Q =>
  
  // Important Values
  // ----------------
  // A single line comment
  // followed by *another one*
  // and yet another one
  val x = 3

  /* A plain comment 
   * continuing on the next line
   *
   * - foo
   * - bar
   */
  def y(c: List[Double]): Int = ???

/*
 * And another code example
 *
 *     val bar = 44
 *
 * That's simple, isn't it?
 */

  /** 
   * A doc comment that will be hidden per default.
   * @param t A parameter
   */
  def z[T](t: T): T = t

}
