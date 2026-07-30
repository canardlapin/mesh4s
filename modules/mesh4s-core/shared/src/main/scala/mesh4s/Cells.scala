package mesh4s

/** The three values attached to one oriented triangle in corner order. */
final case class Triangle[+A](first: A, second: A, third: A)

/** The two vertices of one unoriented edge in canonical order. */
final case class Endpoints[+A](first: A, second: A)
