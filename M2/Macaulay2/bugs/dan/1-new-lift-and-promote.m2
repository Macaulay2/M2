-- now it's time to implement "liftable" in the engine

In-Reply-To: <200606270131.k5R1Vje16349@u00.math.uiuc.edu>
From: Michael Stillman <mike@math.cornell.edu>
Subject: Re: liftable
Date: Mon, 26 Jun 2006 21:51:29 -0400
To: dan@math.uiuc.edu

That sounds good to me.  I'm in the middle of fixing resolutions  
right now, but I'll get to promote and lift after that.  Is that  
timing OK with you?

On Jun 26, 2006, at 9:31 PM, Dan Grayson wrote:

>
> Our function "liftable" gets its answer with "try":
>
>     liftable(RingElement,Ring) := Boolean =>
>     liftable(ZZ,Ring) :=
>     liftable(QQ,Ring) := (f,R) -> try (lift(f,R);true) else false
>
> But that's a good way to mask errors that give the wrong answer.
>
> It would be easy to implement "liftable" in the engine.  We could even
> economize and change all the lift methods to take a result pointer  
> instead a
> result reference, and so that being given a null pointer means we  
> are only
> interested in the boolean return value.
>
> Shall I implement it?  (After you fix lift and promote.)
