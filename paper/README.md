This is a [paper] written as a functional pearl about the general
technique used by the library. It was [rejected] by the [Journal of
Functional Progamming][jfp] but I don't have the time and energy to
significantly rewrite it (see below). 

I think it's readable in its current form if you are an OCaml
programmer and either want to understand how the library works or to
apply the technique on other generic data formats.

[paper]: soup.pdf
[rejected]: jfp-reject.txt
[jfp]: https://www.cambridge.org/core/journals/journal-of-functional-programming


## Rewrite (if ever happens)

- Address reviewer comments and their misunderstandings. 

  Part of the problem is that we wanted to expose a real world
  blueprint of a technique, have it as a pearl and limit the
  exposition to fit on 15 pages (self-imposed).
  
  Now we have the following conflicting complaints:
  
  - It's too technical and detailed for a pearl. It's not "joyful" enough.
    Indeed it's a very boring exposition of the full details it takes to 
    have an ergonomic system in ML for dealing with the serialisation disaster
    that JSON is.
  - There are not enough motivating examples and details about design choices 
    (though honestly there's not thousands ways to construct and deconstruct 
    an array) and we get complaints that we do not reference related works.
  
  So if anything it should likely rather be turned into a regular
  academic paper but I'm not sure it's worth the effort. The document
  as it stands is likely already useful for a motivated individual.

- Like was eventually done in `jsont`, also have an optional
  `unknowns_mems` in `Obj_cases` shapes. This makes the exposition
  slightly more complex though as we need to talk about the overriding
  behaviour.
