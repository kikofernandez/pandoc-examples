# Capabilities status

```{#lst:maybeLinearSupport .encore
    caption="Support for `Maybe`'s do not fully work with `linear` types."}

subord class Two
  var x: int
  def init(): unit
    this.x = 5
  end
end

linear class One: Id
  var sub: Two
  def init(): unit
    this.sub = new Two()
  end
  def x(): int
    this.sub.x
  end
  def setX(x: int): unit
    this.sub.x = x
  end
end

-- this function does not compile. I cannot get it to
-- compile in any possible way

fun moo(var m: Maybe[One]): One
  match consume m with
    case Nothing => consume new One()
    case Just(z) => consume(z)
  end
end

```


In @lst:maybeLinearSupport, if `One` is `read`, then the function `moo` who receives
a `Maybe[One]` works. The distinction between a `read Maybe[One]` and a `linear Maybe[One]`
prevents us from reusing this function, i.e., the annotations of `var` and `val`
are different and the function cannot be polymorphic on its *mode*.

~~~{note=""}
linearity seems to be more related to a property of the object rather than
a mode by itself. this means, @lst:maybeLinearSupport can declare a function that
we know does not use any linear and we could declare another function that may
be `read`, `local` or any other mode that has to be treated *linearly*.
what are the implications of this?
~~~
