package functorial

trait Companion {
  // low priority implicit
  implicit def self: this.type = this
}

trait HasCompanion[+A <: Companion] {
  implicit val F: A
}

