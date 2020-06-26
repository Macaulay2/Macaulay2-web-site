Macaulay2, version 1.10.0.1
with packages: ConwayPolynomials, Elimination, IntegralClosure, InverseSystems, LLLBases, PrimaryDecomposition,
               ReesAlgebra, TangentCone

i1 : QQ[x]

o1 = QQ[x]

o1 : PolynomialRing

i2 : I=ideal x

o2 = ideal x

o2 : Ideal of QQ[x]

i3 : gbTrace =3

o3 = 3

i4 : gb I

   -- registering gb 0 at 0x110bb7700

   -- [gb]{1}(1)mnumber of (nonminimal) gb elements = 1
   -- number of monomials                = 1
   -- #reduction steps = 0
   -- #spairs done = 1
   -- ncalls = 0
   -- nloop = 0
   -- nsaved = 0
   -- 
o4 = GroebnerBasis[status: done; S-pairs encountered up to degree 0]

o4 : GroebnerBasis

i5 : gb I

o5 = GroebnerBasis[status: done; S-pairs encountered up to degree 0]

o5 : GroebnerBasis

i6 : J=ideal x

o6 = ideal x

o6 : Ideal of QQ[x]

i7 : I == J

o7 = true

i8 : I === J

o8 = true

i9 : gb I

o9 = GroebnerBasis[status: done; S-pairs encountered up to degree 0]

o9 : GroebnerBasis

i10 : gb J

   -- registering gb 1 at 0x110bb7540

   -- [gb]{1}(1)mnumber of (nonminimal) gb elements = 1
   -- number of monomials                = 1
   -- #reduction steps = 0
   -- #spairs done = 1
   -- ncalls = 0
   -- nloop = 0
   -- nsaved = 0
   -- 
o10 = GroebnerBasis[status: done; S-pairs encountered up to degree 0]

o10 : GroebnerBasis

i11 : I === J

o11 = true

i12 : methods symbol ==

o12 = {((==, =), Thing, Thing)                                       }
      {((==, =), Type, Type)                                         }
      {(==, BettiTally, BettiTally)                                  }
      {(==, Boolean, Boolean)                                        }
      {(==, CC, CC)                                                  }
      {(==, CC, QQ)                                                  }
      {(==, CC, RR)                                                  }
      {(==, CC, ZZ)                                                  }
      {(==, ChainComplex, ChainComplex)                              }
      {(==, ChainComplex, ZZ)                                        }
      {(==, ChainComplexMap, ChainComplexMap)                        }
      {(==, ChainComplexMap, RingElement)                            }
      {(==, ChainComplexMap, ZZ)                                     }
      {(==, Constant, Constant)                                      }
      {(==, Constant, InexactNumber)                                 }
      {(==, Constant, RingElement)                                   }
      {(==, Equation, Equation)                                      }
      {(==, Equation, Expression)                                    }
      {(==, Equation, Holder)                                        }
      {(==, Expression, Equation)                                    }
      {(==, Expression, Expression)                                  }
      {(==, Expression, Thing)                                       }
      {(==, GradedModule, GradedModule)                              }
      {(==, GradedModuleMap, GradedModuleMap)                        }
      {(==, GradedModuleMap, RingElement)                            }
      {(==, GradedModuleMap, ZZ)                                     }
      {(==, Holder, Equation)                                        }
      {(==, Holder, Holder)                                          }
      {(==, Ideal, Ideal)                                            }
      {(==, Ideal, Module)                                           }
      {(==, Ideal, MonomialIdeal)                                    }
      {(==, Ideal, Ring)                                             }
      {(==, Ideal, ZZ)                                               }
      {(==, InexactNumber, Constant)                                 }
      {(==, InexactNumber, RingElement)                              }
      {(==, InfiniteNumber, InfiniteNumber)                          }
      {(==, InfiniteNumber, Number)                                  }
      {(==, InfiniteNumber, RR)                                      }
      {(==, Matrix, Matrix)                                          }
      {(==, Matrix, Number)                                          }
      {(==, Matrix, RingElement)                                     }
      {(==, Matrix, ZZ)                                              }
      {(==, Module, Ideal)                                           }
      {(==, Module, Module)                                          }
      {(==, Module, ZZ)                                              }
      {(==, MonoidElement, MonoidElement)                            }
      {(==, MonomialIdeal, Ideal)                                    }
      {(==, MonomialIdeal, MonomialIdeal)                            }
      {(==, MonomialIdeal, Ring)                                     }
      {(==, MonomialIdeal, ZZ)                                       }
      {(==, MutableMatrix, MutableMatrix)                            }
      {(==, MutableMatrix, ZZ)                                       }
      {(==, Net, Net)                                                }
      {(==, Net, String)                                             }
      {(==, Nothing, Nothing)                                        }
      {(==, Number, InfiniteNumber)                                  }
      {(==, Number, Matrix)                                          }
      {(==, Number, RingElement)                                     }
      {(==, ProjectiveHilbertPolynomial, ProjectiveHilbertPolynomial)}
      {(==, QQ, CC)                                                  }
      {(==, QQ, QQ)                                                  }
      {(==, QQ, RR)                                                  }
      {(==, QQ, ZZ)                                                  }
      {(==, Ring, Ideal)                                             }
      {(==, Ring, MonomialIdeal)                                     }
      {(==, Ring, ZZ)                                                }
      {(==, RingElement, ChainComplexMap)                            }
      {(==, RingElement, Constant)                                   }
      {(==, RingElement, GradedModuleMap)                            }
      {(==, RingElement, InexactNumber)                              }
      {(==, RingElement, Matrix)                                     }
      {(==, RingElement, Number)                                     }
      {(==, RingElement, RingElement)                                }
      {(==, RingElement, ZZ)                                         }
      {(==, RingMap, ZZ)                                             }
      {(==, RR, CC)                                                  }
      {(==, RR, InfiniteNumber)                                      }
      {(==, RR, QQ)                                                  }
      {(==, RR, RR)                                                  }
      {(==, RR, ZZ)                                                  }
      {(==, Sequence, Sequence)                                      }
      {(==, String, Net)                                             }
      {(==, String, String)                                          }
      {(==, Symbol, Symbol)                                          }
      {(==, Tally, ZZ)                                               }
      {(==, Thing, Expression)                                       }
      {(==, Thing, Thing)                                            }
      {(==, Vector, Vector)                                          }
      {(==, VisibleList, VisibleList)                                }
      {(==, ZZ, CC)                                                  }
      {(==, ZZ, ChainComplex)                                        }
      {(==, ZZ, ChainComplexMap)                                     }
      {(==, ZZ, GradedModuleMap)                                     }
      {(==, ZZ, Ideal)                                               }
      {(==, ZZ, Module)                                              }
      {(==, ZZ, MonomialIdeal)                                       }
      {(==, ZZ, MutableMatrix)                                       }
      {(==, ZZ, QQ)                                                  }
      {(==, ZZ, Ring)                                                }
      {(==, ZZ, RingElement)                                         }
      {(==, ZZ, RingMap)                                             }
      {(==, ZZ, RR)                                                  }
      {(==, ZZ, Tally)                                               }
      {(==, ZZ, ZZ)                                                  }

o12 : VerticalList

i13 : x === I

o13 = false

i14 : x

o14 = x

o14 : QQ[x]

i15 : I

o15 = ideal x

o15 : Ideal of QQ[x]

i16 : x == I
stdio:16:3:(3): error: no method for binary operator == applied to objects:
--            x (of class QQ[x])
--     ==     ideal x (of class Ideal)

i17 : peek I

o17 = Ideal{cache => CacheTable{...1...}}
            generators => | x |
            ring => QQ[x]

i18 : peek J

o18 = Ideal{cache => CacheTable{...1...}}
            generators => | x |
            ring => QQ[x]

i19 : class I

o19 = Ideal

o19 : Type

i20 : parent Ideal

o20 = HashTable

o20 : Type

i21 : showStructure 

o21 = Thing : BasicList : Command
                          VisibleList : Array
                                        List : VerticalList
                                        Sequence
              Boolean
              CompiledFunctionBody
              Database
              Dictionary : GlobalDictionary
                           LocalDictionary
              File
              Function : CompiledFunction
                         CompiledFunctionClosure : MethodFunction
                         FunctionClosure : CacheFunction
                                           MethodFunctionWithOptions
              FunctionBody
              HashTable : CoherentSheaf
                          Ideal : MonomialIdeal
                          ImmutableType : Module
                          ModuleMap : Matrix
                          MonoidElement
                          MutableHashTable : CacheTable
                                             Descent
                                             GradedModule : ChainComplex
                                             GradedModuleMap : ChainComplexMap
                                             GroebnerBasis
                                             IndexedVariableTable
                                             Package
                                             Resolution
                                             ScriptedFunctor
                                             Type : HeaderType
                                                    Monoid : OrderedMonoid : GeneralOrderedMonoid
                                                    Ring : EngineRing : FractionField
                                                                        GaloisField
                                                                        InexactField : ComplexField
                                                                                       RealField
                                                                        PolynomialRing
                                                                        QuotientRing
                                                    RingFamily : InexactFieldFamily
                                                    SelfInitializingType
                                                    WrapperType
                                             Variety : AffineVariety
                                                       ProjectiveVariety
                          MutableMatrix
                          OptionTable : GroebnerBasisOptions
                          ProjectiveHilbertPolynomial
                          RingMap
                          SheafOfRings
                          Tally : Set
                                  VirtualTally : BettiTally
              Net : String
              NetFile
              Nothing : InexactNumber
                                     *
              Number : InexactNumber : CC
                                       RR
                       QQ
                       ZZ
              Pseudocode
              Symbol : Keyword
              SymbolBody
              Task

o21 : Descent

o23 = Ideal{cache => CacheTable{...1...}}
            generators => | x |
            ring => QQ[x]

i24 : I=ideal x

o24 = ideal x

o24 : Ideal of QQ[x]

i25 : peek I

o25 = Ideal{cache => CacheTable{}}
            generators => | x |
            ring => QQ[x]

i26 : I.cache

o26 = CacheTable{}

o26 : CacheTable

i27 : J.cache

o27 = CacheTable{...1...}

o27 : CacheTable

i28 : peek I.cache

o28 = CacheTable{}

i29 : peek J.cache

o29 = CacheTable{module => image | x |}

i30 : peek J.cache.module

o30 = Module of Vector{cache => CacheTable{...1...}               }
                       generators => | x |
                       numgens => 1
                       RawFreeModule => free(rank 1 degrees = {1})
                       ring => QQ[x]

i31 : peek J.cache.module.cache

o31 = CacheTable{cache => MutableHashTable{}}

i32 : peek J.cache.module.gens.cache

o32 = CacheTable{GroebnerBasisOptions{HardDegreeLimit => null} => GroebnerBasis[status: done; S-pairs encountered up to degree 0]}
                                      Syzygies => false
                                      SyzygyRows => 0
                 image => image | x |
                 isHomogeneous => true

i33 : peek I

o33 = Ideal{cache => CacheTable{}}
            generators => | x |
            ring => QQ[x]

i34 : peek J

o34 = Ideal{cache => CacheTable{...1...}}
            generators => | x |
            ring => QQ[x]

i35 : I.cache === J.cache

o35 = true

i36 : hash I.cache

o36 = 0

i37 : hash J.cache

o37 = 0

i38 : hash I

o38 = -1395922470

i39 : debug Core

i40 : buckets I

o40 = {{}, {(ring, QQ[x]), (generators, | x |)}, {}, {(cache, CacheTable{})}}

o40 : List

i41 : netList oo

      +---------------------+-------------------+
o41 = |                     |                   |
      +---------------------+-------------------+
      |(ring, QQ[x])        |(generators, | x |)|
      +---------------------+-------------------+
      |                     |                   |
      +---------------------+-------------------+
      |(cache, CacheTable{})|                   |
      +---------------------+-------------------+

i42 : peek I

o42 = Ideal{cache => CacheTable{}}
            generators => | x |
            ring => QQ[x]

i43 : hash I 

o43 = -1395922470

i44 : hash J

o44 = -1395922470

i45 : I2=ideal (x,x^2)

                 2
o45 = ideal (x, x )

o45 : Ideal of QQ[x]

i46 : hash I2

o46 = 1964228974

i47 : set { I,J,I2}

                               2
o47 = set {ideal x, ideal (x, x )}

o47 : Set

i48 : I#aasdf=333
stdio:48:8:(3): error: attempted to modify an immutable hash table

i49 : t = new MutableHashTable 

o49 = MutableHashTable{}

o49 : MutableHashTable

i50 : t#aasdf=333

o50 = 333

i51 : t

o51 = MutableHashTable{...1...}

o51 : MutableHashTable

i52 : t#a=t

o52 = MutableHashTable{...2...}

o52 : MutableHashTable

i53 : peek t

o53 = MutableHashTable{a => MutableHashTable{...2...}}
                       aasdf => 333

i54 : peek'_2 t

o54 = MutableHashTable{a => MutableHashTable{a => MutableHashTable{...2...}}}
                                             aasdf => 333
                       aasdf => 333

i55 : peek'_3 t

o55 = MutableHashTable{a => MutableHashTable{a => MutableHashTable{a => MutableHashTable{...2...}}}}
                                                                   aasdf => 333
                                             aasdf => 333
                       aasdf => 333

i56 : t = new MutableHashTable ; time for i to 1000 do t#i = i^2
     -- used 0.000565126 seconds

i58 : t = new MutableHashTable ; time for i to 10000 do t#i = i^2
     -- used 0.00386217 seconds

i60 : t = new MutableHashTable ; time for i to 100000 do t#i = i^2
     -- used 0.0380831 seconds

i62 : t = {} ; time for i to 10 do t = append(t,i)
     -- used 0.000026329 seconds

i64 : t

o64 = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10}

o64 : List

i65 : t = {} ; time for i to 1000 do t = append(t,i)
     -- used 0.00807162 seconds

i67 : t = {} ; time for i to 10000 do t = append(t,i)
     -- used 0.828334 seconds

i69 : t = new MutableList ; time for i to 10 do t#i = i^2
     -- used 0.000013174 seconds

i71 : peek t

o71 = MutableList{0, 1, 4, 9, 16, 25, 36, 49, 64, 81, 100}

i72 : t#11=11^2

o72 = 121

i73 : peek t

o73 = MutableList{0, 1, 4, 9, 16, 25, 36, 49, 64, 81, 100, 121}

i74 : t = new MutableList ; time for i to 1000 do t#i = i^2
     -- used 0.00213168 seconds

i76 : t = new MutableList ; time for i to 10000 do t#i = i^2
     -- used 0.353865 seconds

i78 : Ideal

o78 = Ideal

o78 : Type

i79 : peek Ideal

o79 = Type of HashTable{(%, Matrix, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/classes.m2:60:48-60:55]*}                                                               }
                        (%, Number, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix2.m2:352:25-356:111]*}
                        (%, RingElement, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix2.m2:347:30-351:93]*}
                        (*, Ideal, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/classes.m2:60:48-60:55]*}
                        (*, Ideal, Module) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/classes.m2:60:48-60:55]*}
                        (*, Ideal, Ring) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:293:32-293:85]*}
                        (*, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:471:39-471:65]*}
                        (*, Ideal, Vector) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:3:25-4:32]*}
                        (*, Ideal, ZZ) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:473:21-473:46]*}
                        (*, Ring, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:294:32-294:85]*}
                        (*, RingElement, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:470:39-470:65]*}
                        (*, ZZ, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:472:21-472:46]*}
                        (+, Ideal, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/classes.m2:60:48-60:55]*}
                        (+, Ideal, Number) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/classes.m2:60:48-60:55]*}
                        (+, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/classes.m2:60:48-60:55]*}
                        (+, Number, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/classes.m2:60:48-60:55]*}
                        (+, RingElement, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/classes.m2:60:48-60:55]*}
                        (/, EngineRing, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/quotring.m2:188:45-236:6]*}
                        (/, Ideal, Function) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:477:35-477:74]*}
                        (/, Ideal, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:504:34-504:55]*}
                        (/, Module, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:505:35-505:47]*}
                        (/, Ring, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/quotring.m2:176:39-181:6]*}
                        (:, Ideal, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/colon.m2:162:33-162:47]*}
                        (:, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/colon.m2:163:39-163:53]*}
                        (:, Module, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/colon.m2:164:35-164:49]*}
                        (==, Ideal, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:547:25-553:40]*}
                        (==, Ideal, Module) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:556:26-556:41]*}
                        (==, Ideal, Ring) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:540:24-543:17]*}
                        (==, Ideal, ZZ) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:297:22-302:11]*}
                        (==, Module, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:557:26-557:41]*}
                        (==, Ring, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:545:24-545:32]*}
                        (==, ZZ, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:296:22-296:30]*}
                        (\, Function, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:478:35-478:74]*}
                        (^, Ideal, Array) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:595:25-607:25]*}
                        (^, Ideal, ZZ) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:512:30-512:67]*}
                        (_, Ideal, List) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:536:23-536:37]*}
                        (_, Ideal, ZZ) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:521:36-521:57]*}
                        ({*Function*}, Ring, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/newring.m2:219:35-219:40]*}
                        (analyticSpread, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:398:51-398:78]*}
                        (associatedGradedRing, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:219:48-219:73]*}
                        (basis, InfiniteNumber, InfiniteNumber, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:784:28-784:65]*}
                        (basis, InfiniteNumber, List, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:784:28-784:65]*}
                        (basis, InfiniteNumber, ZZ, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:784:28-784:65]*}
                        (basis, List, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:773:46-773:79]*}
                        (basis, List, InfiniteNumber, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:784:28-784:65]*}
                        (basis, List, List, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:784:28-784:65]*}
                        (basis, List, ZZ, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:784:28-784:65]*}
                        (basis, ZZ, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:773:46-773:79]*}
                        (basis, ZZ, InfiniteNumber, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:784:28-784:65]*}
                        (basis, ZZ, List, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:784:28-784:65]*}
                        (basis, ZZ, ZZ, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:784:28-784:65]*}
                        (coerce, Ideal, Ring) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/classes.m2:60:48-60:55]*}
                        (coerce, Ring, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/newring.m2:147:33-147:42]*}
                        (distinguished, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:427:47-433:39]*}
                        (distinguishedAndMult, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:491:54-500:60]*}
                        (eliminate, Ideal, List) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/Elimination.m2:95:34-95:49]*}
                        (eliminate, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/Elimination.m2:94:41-94:58]*}
                        (eliminate, List, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/Elimination.m2:83:34-91:26]*}
                        (eliminate, RingElement, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/Elimination.m2:96:40-96:57]*}
                        (endomorphisms, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/IntegralClosure.m2:533:43-545:9]*}
                        (Ext, Ideal, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/ext.m2:184:28-184:51]*}
                        (Ext, Ideal, Module) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/ext.m2:185:29-185:45]*}
                        (Ext, Ideal, Ring) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/ext.m2:183:27-183:45]*}
                        (Ext, Module, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/ext.m2:182:29-182:45]*}
                        (Ext, ZZ, Ideal, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/ext.m2:79:31-79:69]*}
                        (Ext, ZZ, Ideal, Matrix) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/ext.m2:73:32-73:63]*}
                        (Ext, ZZ, Ideal, Module) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/ext.m2:80:32-80:63]*}
                        (Ext, ZZ, Ideal, Ring) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/ext.m2:78:30-78:63]*}
                        (Ext, ZZ, Matrix, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/ext.m2:75:32-75:63]*}
                        (Ext, ZZ, Module, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/ext.m2:77:32-77:63]*}
                        (Fano, ZZ, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/fano.m2:60:35-68:12]*}
                        (Fano, ZZ, Ideal, Ring) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/fano.m2:9:42-57:10]*}
                        (getMinimalPoly, Ideal, RingElement, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/PrimaryDecomposition/radical.m2:37:58-44:9]*}
                        (greedySat, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/PrimaryDecomposition/GTZ.m2:133:39-149:13]*}
                        (hilbertFunction, List, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:188:70-195:22]*}
                        (hilbertFunction, ZZ, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:186:94-186:117]*}
                        (Hom, Ideal, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:483:38-483:62]*}
                        (Hom, Ideal, Module) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:484:39-484:56]*}
                        (Hom, Ideal, Ring) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:489:37-489:56]*}
                        (Hom, Module, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:485:39-485:56]*}
                        (Hom, Ring, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:490:37-490:56]*}
                        (homogenize, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix2.m2:428:49-428:79]*}
                        (idealizer, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/IntegralClosure.m2:503:37-528:67]*}
                        (installHilbertFunction, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/gb.m2:453:53-453:90]*}
                        (integralClosure, Ideal, ZZ) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/IntegralClosure.m2:892:36-908:21]*}
                        (isLinearType, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:199:42-205:24]*}
                        (isPrimary, Ideal, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/PrimaryDecomposition.m2:135:33-137:11]*}
                        (isReduction, Ideal, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:329:29-336:20]*}
                        (isReduction, Ideal, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:339:41-346:20]*}
                        (isSubset, Ideal, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:848:32-848:61]*}
                        (isSubset, Ideal, Module) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:850:33-850:55]*}
                        (isSubset, Module, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:849:33-849:55]*}
                        (leadTerm, ZZ, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:533:39-533:67]*}
                        (lift, Ideal, QQ) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:681:33-684:87]*}
                        (lift, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix2.m2:638:42-642:66]*}
                        (lift, Ideal, ZZ) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:681:33-684:87]*}
                        (localize, Ideal, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/PrimaryDecomposition/Eisenbud-Huneke-Vasconcelos.m2:184:38-193:31]*}
                        (multiplicity, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:240:48-245:27]*}
                        (normalCone, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:212:37-214:19]*}
                        (primaryComponent, Ideal, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/PrimaryDecomposition/Eisenbud-Huneke-Vasconcelos.m2:250:45-256:43]*}
                        (promote, Ideal, Number) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:457:37-457:68]*}
                        (promote, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:457:37-457:68]*}
                        (quotient, Ideal, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/colon.m2:157:48-157:83]*}
                        (quotient, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/colon.m2:158:48-158:89]*}
                        (quotient, Module, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/colon.m2:159:48-159:84]*}
                        (rad, Ideal, ZZ) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/PrimaryDecomposition/radical.m2:107:38-135:11]*}
                        (radical00, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/PrimaryDecomposition/radical.m2:61:39-77:6]*}
                        (reduceLinears, Ideal, Set) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/minPres.m2:291:31-321:28]*}
                        (reductionNumber, Ideal, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:570:40-584:6]*}
                        (reesAlgebra, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:178:38-178:71]*}
                        (reesIdeal, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:158:45-159:26]*}
                        (saturate, Ideal, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/colon.m2:288:40-356:31]*}
                        (saturate, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/colon.m2:359:47-359:78]*}
                        (saturate, Module, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/colon.m2:363:42-370:5]*}
                        (saturate, Vector, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/colon.m2:373:42-373:84]*}
                        (saturation, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/PrimaryDecomposition/GTZ.m2:158:40-158:55]*}
                        (specialFiber, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:314:37-321:21]*}
                        (specialFiberIdeal, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:292:42-294:51]*}
                        (substitute, Ideal, List) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/ringmap.m2:395:42-395:63]*}
                        (substitute, Ideal, Matrix) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/ringmap.m2:313:44-313:70]*}
                        (substitute, Ideal, Option) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/ringmap.m2:401:35-401:58]*}
                        (substitute, Ideal, Ring) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/ringmap.m2:317:42-317:61]*}
                        (substitute, Ideal, RingFamily) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/ringmap.m2:324:48-324:73]*}
                        (toDual, ZZ, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/InverseSystems.m2:112:24-122:10]*}
                        (Tor, ZZ, Ideal, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/tor.m2:50:31-50:69]*}
                        (Tor, ZZ, Ideal, Matrix) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/tor.m2:44:32-44:63]*}
                        (Tor, ZZ, Ideal, Module) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/tor.m2:51:32-51:63]*}
                        (Tor, ZZ, Ideal, Ring) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/tor.m2:49:30-49:63]*}
                        (Tor, ZZ, Matrix, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/tor.m2:46:32-46:63]*}
                        (Tor, ZZ, Module, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/tor.m2:48:32-48:63]*}
                        (truncate, List, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:829:42-829:71]*}
                        (truncate, ZZ, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:831:40-831:58]*}
                        (vasconcelos, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/IntegralClosure.m2:549:41-565:9]*}
                        _* => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:476:14-476:42]*}
                        {Standard, AfterNoPrint} => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:507:67-509:88]*}
                        {Standard, AfterPrint} => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:507:67-509:88]*}
                        analyticSpread => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:395:37-395:62]*}
                        annihilator => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:622:33-622:63]*}
                        associatedGradedRing => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:218:35-218:54]*}
                        associatedPrimes => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/PrimaryDecomposition/Eisenbud-Huneke-Vasconcelos.m2:69:37-69:52]*}
                        basis => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:802:21-802:44]*}
                        betti => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/chaincomplexes.m2:716:21-716:49]*}
                        codim => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/option.m2:6:20-8:34]*}
                        comodule => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:461:31-461:54]*}
                        containsDthPowers => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/InverseSystems.m2:98:29-108:5]*}
                        decompose => {*a cache function*}
                        degree => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:519:19-519:49]*}
                        degreeLength => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:453:25-453:46]*}
                        degrees => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:452:20-452:49]*}
                        degreesRing => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:454:24-454:44]*}
                        dim => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:515:16-515:43]*}
                        distinguished => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:418:35-424:39]*}
                        distinguishedAndMult => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:480:42-489:60]*}
                        engineMGBF4 => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/gb.m2:121:27-121:65]*}
                        euler => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:468:21-468:41]*}
                        eulers => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:467:22-467:43]*}
                        expression => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:444:25-444:122]*}
                        flattenRing => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/newring.m2:222:27-227:50]*}
                        gb => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/gb.m2:190:35-190:60]*}
                        gbBoolean => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/gb.m2:470:31-470:89]*}
                        gbRemove => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/gb.m2:462:42-464:74]*}
                        gbSnapshot => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/gb.m2:459:65-459:109]*}
                        genera => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:464:21-464:43]*}
                        generator => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:481:39-491:6]*}
                        generators => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:475:36-475:48]*}
                        genus => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:465:20-465:41]*}
                        groebnerBasis => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/gb.m2:130:29-130:65]*}
                        GTZ0 => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/PrimaryDecomposition/GTZ.m2:161:19-167:25]*}
                        GTZ1 => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/PrimaryDecomposition/GTZ.m2:170:19-183:6]*}
                        hilbertPolynomial => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:206:36-206:77]*}
                        hilbertSeries => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:137:29-137:64]*}
                        idealPrepare => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:589:25-589:29]*}
                        independentSets => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/monideal.m2:163:28-163:70]*}
                        integralClosure => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/IntegralClosure.m2:910:32-910:60]*}
                        integralClosures => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/IntegralClosure.m2:913:33-929:75]*}
                        inverseSystem => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/InverseSystems.m2:129:25-137:35]*}
                        irreducibleCharacteristicSeries => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/factor.m2:65:44-84:64]*}
                        isHomogeneous => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:450:28-450:56]*}
                        isIdeal => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:449:20-449:23]*}
                        isLinearType => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:190:25-196:24]*}
                        isMonomialIdeal => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/monideal.m2:196:30-196:136]*}
                        isPrimary => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/PrimaryDecomposition.m2:134:23-134:47]*}
                        isPrime => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/factor.m2:163:20-163:66]*}
                        jacobian => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:534:33-534:56]*}
                        leadTerm => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:532:33-532:59]*}
                        mathML => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/mathml.m2:174:19-179:77]*}
                        mingens => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:503:36-503:63]*}
                        minimalBetti => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/option.m2:6:20-8:34]*}
                        minimalPresentation => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/minPres.m2:412:59-414:23]*}
                        minimalPrimes => {*a cache function*}
                        minimalReduction => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:505:38-526:6]*}
                        module => {*a cache function*}
                        monomialIdeal => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/monideal.m2:180:46-180:77]*}
                        monomialSubideal => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/monideal.m2:309:31-322:34]*}
                        multidegree => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:357:24-357:59]*}
                        multiplicity => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:233:32-238:27]*}
                        net => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:445:18-445:36]*}
                        newdecompose => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/PrimaryDecomposition/Shimoyama-Yokoyama.m2:98:36-98:53]*}
                        normalCone => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:208:24-210:19]*}
                        numgens => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:531:22-531:51]*}
                        parametersInIdeal => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/IntegralClosure.m2:936:30-961:13]*}
                        PD => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/PrimaryDecomposition/GTZ.m2:33:17-43:41]*}
                        poincare => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:535:23-535:44]*}
                        primaryDecomposition => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/PrimaryDecomposition.m2:114:41-130:32]*}
                        primaryDecompositionGTZ => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/PrimaryDecomposition/GTZ.m2:11:38-18:17]*}
                        prune => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/minPres.m2:412:59-414:23]*}
                        quotient => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:462:34-462:53]*}
                        rad => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/PrimaryDecomposition/radical.m2:81:22-104:11]*}
                        radical => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/radical.m2:147:35-161:11]*}
                        randomKRationalPoint => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/varieties.m2:451:33-481:6]*}
                        reesAlgebra => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:168:24-168:55]*}
                        reesIdeal => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:143:31-144:51]*}
                        regularity => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/chaincomplexes.m2:648:26-651:65]*}
                        removeLowestDimension => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/radical.m2:35:45-52:20]*}
                        resolution => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/res.m2:228:45-232:6]*}
                        ring => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:538:19-538:24]*}
                        saturate => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/colon.m2:361:33-361:73]*}
                        singularLocus => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/quotring.m2:308:45-308:71]*}
                        specialFiber => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:303:25-310:21]*}
                        specialFiberIdeal => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:286:30-288:51]*}
                        support => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix2.m2:381:22-381:95]*}
                        synonym => ideal
                        tangentCone => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/TangentCone.m2:20:29-36:11]*}
                        toExternalString => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:447:31-447:73]*}
                        topComponents => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/radical.m2:59:37-62:47]*}
                        toString => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:446:23-446:46]*}
                        trim => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:520:29-520:96]*}
                        universalEmbedding => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:108:45-111:24]*}
                        variety => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/varieties.m2:133:20-133:35]*}
                        whichGm => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:588:20-610:19]*}
                        Wrap => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/last.m2:22:114-22:124]*}
                        ZZp => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/quotring.m2:56:19-92:11]*}

i80 : methods Thing

o80 = {(!, Thing)                                     }
      {(#, HashTable, Thing)                          }
      {(#?, HashTable, Thing)                         }
      {(#?, Set, Thing)                               }
      {(%, Thing, Thing)                              }
      {(&, Thing, Thing)                              }
      {((%, =), Thing, Thing)                         }
      {((&, =), Thing, Thing)                         }
      {((*), Thing)                                   }
      {((**, =), Thing, Thing)                        }
      {((*, =), Thing, Thing)                         }
      {((++, =), Thing, Thing)                        }
      {((+, =), Thing, Thing)                         }
      {((-, =), Thing, Thing)                         }
      {((.., =), Thing, Thing)                        }
      {((..<, =), Thing, Thing)                       }
      {((/, =), Thing, Thing)                         }
      {((//, =), Thing, Thing)                        }
      {((:, =), Thing, Thing)                         }
      {((<<, =), Thing, Thing)                        }
      {((<==, =), Thing, Thing)                       }
      {((<===, =), Thing, Thing)                      }
      {((<==>, =), Thing, Thing)                      }
      {((==, =), Thing, Thing)                        }
      {((===>, =), Thing, Thing)                      }
      {((==>, =), Thing, Thing)                       }
      {((>>, =), Thing, Thing)                        }
      {((?, =), Thing, Thing)                         }
      {((@, =), Thing, Thing)                         }
      {((@@, =), Thing, Thing)                        }
      {((\, =), Thing, Thing)                         }
      {((\\, =), Thing, Thing)                        }
      {((^**, =), Thing, Thing)                       }
      {((^, =), Thing, Thing)                         }
      {((^^, =), Thing, Thing)                        }
      {((_, =), IndexedVariableTable, Thing)          }
      {((_, =), Symbol, Thing)                        }
      {((_, =), Thing, Thing)                         }
      {((|, =), Thing, Thing)                         }
      {((|-, =), Thing, Thing)                        }
      {((||, =), Thing, Thing)                        }
      {((and, =), Thing, Thing)                       }
      {((or, =), Thing, Thing)                        }
      {((SPACE, =), Function, Thing)                  }
      {((SPACE, =), Thing, Thing)                     }
      {(**, Expression, Thing)                        }
      {(**, Thing, Expression)                        }
      {(**, Thing, InexactFieldFamily)                }
      {(**, Thing, Thing)                             }
      {(*, Expression, Thing)                         }
      {(*, Thing)                                     }
      {(*, Thing, Expression)                         }
      {(*, Thing, List)                               }
      {(*, Thing, Thing)                              }
      {(++, Thing, Thing)                             }
      {(+, Expression, Thing)                         }
      {(+, Thing)                                     }
      {(+, Thing, Expression)                         }
      {(+, Thing, Thing)                              }
      {(-, Expression, Thing)                         }
      {(-, Thing)                                     }
      {(-, Thing, Expression)                         }
      {(-, Thing, Thing)                              }
      {(.., Expression, Thing)                        }
      {(.., IndexedVariableTable, Thing)              }
      {(.., MonoidElement, Thing)                     }
      {(.., RingElement, Thing)                       }
      {(.., Subscript, Thing)                         }
      {(.., Thing, Expression)                        }
      {(.., Thing, IndexedVariableTable)              }
      {(.., Thing, MonoidElement)                     }
      {(.., Thing, RingElement)                       }
      {(.., Thing, Subscript)                         }
      {(.., Thing, Thing)                             }
      {(..<, Expression, Thing)                       }
      {(..<, IndexedVariableTable, Thing)             }
      {(..<, MonoidElement, Thing)                    }
      {(..<, RingElement, Thing)                      }
      {(..<, Subscript, Thing)                        }
      {(..<, Thing, Expression)                       }
      {(..<, Thing, IndexedVariableTable)             }
      {(..<, Thing, MonoidElement)                    }
      {(..<, Thing, RingElement)                      }
      {(..<, Thing, Subscript)                        }
      {(..<, Thing, Thing)                            }
      {(/, Expression, Thing)                         }
      {(/, List, Thing)                               }
      {(/, Thing, Expression)                         }
      {(/, Thing, Thing)                              }
      {(//, Thing, Command)                           }
      {(//, Thing, Function)                          }
      {(//, Thing, SelfInitializingType)              }
      {(//, Thing, Thing)                             }
      {(:, Expression, Thing)                         }
      {(:, Thing, Expression)                         }
      {(:, Thing, Thing)                              }
      {(:, ZZ, Thing)                                 }
      {(<, Thing)                                     }
      {(<, Thing, Thing)                              }
      {(<<, File, Thing)                              }
      {(<<, List, Thing)                              }
      {(<<, Nothing, Thing)                           }
      {(<<, String, Thing)                            }
      {(<<, Thing)                                    }
      {(<<, Thing, Thing)                             }
      {(<=, Thing)                                    }
      {(<=, Thing, Thing)                             }
      {(<==, Thing)                                   }
      {(<==, Thing, Thing)                            }
      {(<===, Thing)                                  }
      {(<===, Thing, Thing)                           }
      {(<==>, Thing, Thing)                           }
      {(=!=, Thing, Thing)                            }
      {(==, Expression, Thing)                        }
      {(==, Thing, Expression)                        }
      {(==, Thing, Thing)                             }
      {(===, Thing, Thing)                            }
      {(===>, Thing, Thing)                           }
      {(==>, Thing, Thing)                            }
      {(=>, Thing, Thing)                             }
      {(>, Thing)                                     }
      {(>, Thing, Thing)                              }
      {(>=, Thing)                                    }
      {(>=, Thing, Thing)                             }
      {(>>, Thing, Thing)                             }
      {(?, InfiniteNumber, Thing)                     }
      {(?, Thing)                                     }
      {(?, Thing, InfiniteNumber)                     }
      {(?, Thing, Thing)                              }
      {(@, Thing, Thing)                              }
      {(@@, Thing, Thing)                             }
      {(\, Thing, Thing)                              }
      {(\\, Command, Thing)                           }
      {(\\, Function, Thing)                          }
      {(\\, SelfInitializingType, Thing)              }
      {(\\, Thing, Thing)                             }
      {(^**, Thing, Thing)                            }
      {(^*, Thing)                                    }
      {(^, Expression, Thing)                         }
      {(^, ScriptedFunctor, Thing)                    }
      {(^, Thing, Expression)                         }
      {(^, Thing, Thing)                              }
      {(^^, Thing, Thing)                             }
      {(_*, Thing)                                    }
      {(_, Expression, Thing)                         }
      {(_, Function, Thing)                           }
      {(_, IndexedVariableTable, Thing)               }
      {(_, ScriptedFunctor, Thing)                    }
      {(_, Symbol, Thing)                             }
      {(_, Tally, Thing)                              }
      {(_, Thing, Expression)                         }
      {(_, Thing, Thing)                              }
      {({*Function*}, Ring, Thing)                    }
      {({*Function*}, Symbol, Thing)                  }
      {({*Function*}, Thing)                          }
      {({*Function*}, Thing)                          }
      {({*Function*}, Thing)                          }
      {({*Function*}, Thing)                          }
      {({*Function*}, Thing)                          }
      {({*Function*}, Thing)                          }
      {({*Function*}, Thing)                          }
      {({*Function*}, Thing)                          }
      {({*Function*}, Thing)                          }
      {({*Function*}, Thing)                          }
      {({*Function*}, Thing)                          }
      {({*Function*}, Thing)                          }
      {({*Function*}, Thing)                          }
      {({*Function*}, Thing)                          }
      {({*Function*}, Thing)                          }
      {({*Function*}, Thing)                          }
      {({*Function*}, Thing)                          }
      {({*Function*}, Thing, Function)                }
      {({*Function*}, Thing, Function)                }
      {({*Function*}, Thing, Function)                }
      {({*Function*}, Thing, Sequence)                }
      {({*Function*}, Thing, Thing)                   }
      {({*Function*}, Thing, Thing, Sequence)         }
      {({*Function*}, Thing, Thing, Thing)            }
      {(|, Thing, Thing)                              }
      {(|-, Thing)                                    }
      {(|-, Thing, Thing)                             }
      {(||, Thing, Thing)                             }
      {(~, Thing)                                     }
      {(accumulate, Function, Thing, VisibleList)     }
      {(accumulate, VisibleList, Thing, Function)     }
      {(addHook, HashTable, Thing, Function)          }
      {(addHook, MutableHashTable, Thing, Function)   }
      {(and, Thing, Thing)                            }
      {(append, BasicList, Thing)                     }
      {(baseName, Thing)                              }
      {(between, Thing, VisibleList)                  }
      {(briefDocumentation, Thing)                    }
      {(coerce, Thing, Nothing)                       }
      {(coerce, Thing, Thing)                         }
      {(createTask, Function, Thing)                  }
      {(describe, Thing)                              }
      {(dictionary, Thing)                            }
      {(examples, Thing)                              }
      {(expression, Thing)                            }
      {(fold, Function, Thing, VisibleList)           }
      {(fold, VisibleList, Thing, Function)           }
      {(formatDocumentTag, Thing)                     }
      {(headline, Thing)                              }
      {(help, Thing)                                  }
      {(hold, Thing)                                  }
      {(html, Thing)                                  }
      {(htmlFilename, Thing)                          }
      {(idealPrepare, Thing)                          }
      {(insert, ZZ, Thing, VisibleList)               }
      {(instance, Thing, Type)                        }
      {(isDocumentableMethod, Thing)                  }
      {(isDocumentableThing, Thing)                   }
      {(isFreeModule, Thing)                          }
      {(isIdeal, Thing)                               }
      {(isModule, Thing)                              }
      {(isMonomialIdeal, Thing)                       }
      {(isNumber, Thing)                              }
      {(isPolynomialRing, Thing)                      }
      {(isQuotientModule, Thing)                      }
      {(isRing, Thing)                                }
      {(isSubmodule, Thing)                           }
      {(makeDocumentTag, Thing)                       }
      {(makeExampleItem, Thing)                       }
      {(map, RingFamily, Thing, Thing)                }
      {(map, Thing, RingFamily, Thing)                }
      {(mathML, Thing)                                }
      {(member, Thing, Set)                           }
      {(member, Thing, VisibleList)                   }
      {(methods, Thing)                               }
      {(mutable, Thing)                               }
      {(net, Thing)                                   }
      {(NewFromMethod, MarkUpType, Thing)             }
      {(NewFromMethod, TO, Thing)                     }
      {(NewFromMethod, TOH, Thing)                    }
      {(not, Thing)                                   }
      {(or, Thing, Thing)                             }
      {(package, Thing)                               }
      {(packageKey, Thing, String)                    }
      {(packageKey0, Thing)                           }
      {(peek', ZZ, Thing)                             }
      {(precedence, Thing)                            }
      {(prepend, Thing, BasicList)                    }
      {(pretty, Thing)                                }
      {(pretty2, Thing)                               }
      {(remove, HashTable, Thing)                     }
      {(removeHook, HashTable, Thing, Function)       }
      {(removeHook, MutableHashTable, Thing, Function)}
      {(replace, ZZ, Thing, VisibleList)              }
      {(rightPrecedence, Thing)                       }
      {(rle, Thing)                                   }
      {(runHooks, HashTable, Thing, Thing)            }
      {(runHooks, MutableHashTable, Thing, Thing)     }
      {(runHooks, Symbol, Thing)                      }
      {(schedule, Function, Thing)                    }
      {(showTex, Thing)                               }
      {(SPACE, Command, Thing)                        }
      {(SPACE, Expression, Thing)                     }
      {(SPACE, Function, Thing)                       }
      {(SPACE, ScriptedFunctor, Thing)                }
      {(SPACE, SelfInitializingType, Thing)           }
      {(SPACE, Thing, Expression)                     }
      {(SPACE, Thing, Thing)                          }
      {(SPACE, WrapperType, Thing)                    }
      {(startsWithSymbol, Thing)                      }
      {(substitute, Divide, Thing)                    }
      {(substitute, Power, Thing)                     }
      {(substitute, Product, Thing)                   }
      {(substitute, Sum, Thing)                       }
      {(succ, Thing, Thing)                           }
      {(SYNOPSIS, Thing)                              }
      {(tex, Thing)                                   }
      {(texMath, Thing)                               }
      {(toExternalFormat, Thing)                      }
      {(toExternalString, Thing)                      }
      {(toString', Function, Thing)                   }
      {(toString, Thing)                              }
      {(ultimate, Function, Thing)                    }
      {(undocumented, Thing)                          }
      {(use, Thing)                                   }
      {(value', Thing)                                }
      {(verifyKey, Thing)                             }

o80 : VerticalList

i81 : f = x -> x

o81 = f

o81 : FunctionClosure

i82 : f_2 3

o82 = (2, 3)

o82 : Sequence

i83 : f_2 (3,4)

o83 = (2, 3, 4)

o83 : Sequence

i84 : symmetricPower_3 M
stdio:75:15:(3): error: no method found for applying symmetricPower to:
     argument 1 :  3 (of class ZZ)
     argument 2 :  M (of class Symbol)

i85 : R = QQ[x]

   -- registering polynomial ring 3 at 0x10d68ad00

o85 = R

o85 : PolynomialRing

i86 : symmetricPower_3 R^1

   -- registering polynomial ring 4 at 0x10d68ab00

--- component 2 -----
gb elem = <2>-<0>
--- component 3 -----
gb elem = <3>-<1>



   -- registering mutable matrix 0 at 0x111114080

   -- registering mutable matrix 1 at 0x111188e40

   -- registering polynomial ring 5 at 0x10d68a900

   -- registering polynomial ring 6 at 0x10d68a600

   -- registering gb 2 at 0x110bb7e00

   -- [gb]number of (nonminimal) gb elements = 0
   -- number of monomials                = 0
   -- #reduction steps = 0
   -- #spairs done = 0
   -- ncalls = 0
   -- nloop = 0
   -- nsaved = 0
   -- 
   -- registering gb 3 at 0x110bb7c40

   -- [gb]{3}(1)mnumber of (nonminimal) gb elements = 1
   -- number of monomials                = 2
   -- #reduction steps = 0
   -- #spairs done = 1
   -- ncalls = 0
   -- nloop = 0
   -- nsaved = 0
   -- 
   -- registering gb 4 at 0x110bb7a80
number of (nonminimal) gb elements = 0
   -- number of monomials                = 0
   -- #reduction steps = 0
   -- #spairs done = 0
   -- ncalls = 0
   -- nloop = 0
   -- nsaved = 0
   -- 
   -- registering gb 5 at 0x110bb78c0

   -- [gb]{0}(1)mnumber of (nonminimal) gb elements = 1
   -- number of monomials                = 2
   -- #reduction steps = 0
   -- #spairs done = 1
   -- ncalls = 0
   -- nloop = 0
   -- nsaved = 0
   -- 
   -- registering gb 6 at 0x110bb7380

   -- [gb]{0}(1)mnumber of (nonminimal) gb elements = 1
   -- number of monomials                = 2
   -- #reduction steps = 0
   -- #spairs done = 1
   -- ncalls = 0
   -- nloop = 0
   -- nsaved = 0
   -- 
   -- registering gb 7 at 0x110bb71c0

   -- [gb]number of (nonminimal) gb elements = 0
   -- number of monomials                = 0
   -- #reduction steps = 0
   -- #spairs done = 0
   -- ncalls = 0
   -- nloop = 0
   -- nsaved = 0
   -- 
   -- registering gb 8 at 0x110bb7000

   -- [gb]number of (nonminimal) gb elements = 0
   -- number of monomials                = 0
   -- #reduction steps = 0
   -- #spairs done = 0
   -- ncalls = 0
   -- nloop = 0
   -- nsaved = 0
   -- 
   -- registering gb 9 at 0x10f6d1e00

   -- [gb]number of (nonminimal) gb elements = 0
   -- number of monomials                = 0
   -- #reduction steps = 0
   -- #spairs done = 0
   -- ncalls = 0
   -- nloop = 0
   -- nsaved = 0
   -- 
   -- registering gb 10 at 0x10f6d1c40
number of (nonminimal) gb elements = 0
   -- number of monomials                = 0
   -- #reduction steps = 0
   -- #spairs done = 0
   -- ncalls = 0
   -- nloop = 0
   -- nsaved = 0
   -- 
   -- registering gb 11 at 0x10f6d1a80

   -- [gb]number of (nonminimal) gb elements = 0
   -- number of monomials                = 0
   -- #reduction steps = 0
   -- #spairs done = 0
   -- ncalls = 0
   -- nloop = 0
   -- nsaved = 0
   -- 
   -- registering gb 12 at 0x10f6d18c0

   -- [gb]number of (nonminimal) gb elements = 0
   -- number of monomials                = 0
   -- #reduction steps = 0
   -- #spairs done = 0
   -- ncalls = 0
   -- nloop = 0
   -- nsaved = 0
   -- 
   -- registering gb 13 at 0x10f6d1700

   -- [gb]number of (nonminimal) gb elements = 0
   -- number of monomials                = 0
   -- #reduction steps = 0
   -- #spairs done = 0
   -- ncalls = 0
   -- nloop = 0
   -- nsaved = 0
   -- 
   -- registering gb 14 at 0x10f6d1380

   -- [gb]{0}(1)mnumber of (nonminimal) gb elements = 1
   -- number of monomials                = 2
   -- #reduction steps = 0
   -- #spairs done = 1
   -- ncalls = 0
   -- nloop = 0
   -- nsaved = 0
   -- 
   -- registering gb 15 at 0x10f6d11c0

   -- [gb]{0}(1)mnumber of (nonminimal) gb elements = 1
   -- number of monomials                = 2
   -- #reduction steps = 0
   -- #spairs done = 1
   -- ncalls = 0
   -- nloop = 0
   -- nsaved = 0
   -- 
   -- registering gb 16 at 0x10f6d1000

   -- [gb]number of (nonminimal) gb elements = 0
   -- number of monomials                = 0
   -- #reduction steps = 0
   -- #spairs done = 0
   -- ncalls = 0
   -- nloop = 0
   -- nsaved = 0
   -- 
   -- registering gb 17 at 0x10d685c40

   -- [gb]number of (nonminimal) gb elements = 0
   -- number of monomials                = 0
   -- #reduction steps = 0
   -- #spairs done = 0
   -- ncalls = 0
   -- nloop = 0
   -- nsaved = 0
   -- 
   -- registering gb 18 at 0x10d685a80

   -- [gb]number of (nonminimal) gb elements = 0
   -- number of monomials                = 0
   -- #reduction steps = 0
   -- #spairs done = 0
   -- ncalls = 0
   -- nloop = 0
   -- nsaved = 0
   -- 
   -- registering gb 19 at 0x10d6858c0
number of (nonminimal) gb elements = 0
   -- number of monomials                = 0
   -- #reduction steps = 0
   -- #spairs done = 0
   -- ncalls = 0
   -- nloop = 0
   -- nsaved = 0
   -- 
   -- registering gb 20 at 0x10d685540

   -- [gb]number of (nonminimal) gb elements = 0
   -- number of monomials                = 0
   -- #reduction steps = 0
   -- #spairs done = 0
   -- ncalls = 0
   -- nloop = 0
   -- nsaved = 0
   -- 
       1
o86 = R

o86 : R-module, free

i87 : f = x -> return 1

o87 = f

o87 : FunctionClosure

i88 : f = x -> return (1)

o88 = f

o88 : FunctionClosure

i89 : I2

                 2
o89 = ideal (x, x )

o89 : Ideal of QQ[x]

i90 : flatten entries gens gb I2

   -- registering gb 21 at 0x10d685380

   -- [gb]{1}(1)m{2}(1)onumber of (nonminimal) gb elements = 1
   -- number of monomials                = 1
   -- #reduction steps = 1
   -- #spairs done = 2
   -- ncalls = 0
   -- nloop = 0
   -- nsaved = 0
   -- 
o90 = {x}

o90 : List

i91 : gbTrace =0

o91 = 0

i92 : flatten entries gens gb I2

o92 = {x}

o92 : List

i93 : gb I2

o93 = GroebnerBasis[status: done; S-pairs encountered up to degree 1]

o93 : GroebnerBasis

i94 : gens oo

o94 = | x |

                    1             1
o94 : Matrix (QQ[x])  <--- (QQ[x])

i95 : entries oo

o95 = {{x}}

o95 : List

i96 : flatten oo

o96 = {x}

o96 : List

i97 : first ooo

o97 = {x}

o97 : List

i98 : I2_*

           2
o98 = {x, x }

o98 : List

i99 : (gb I2)_*
stdio:90:8:(3): error: no method for postfix operator _* applied to object:
--            GroebnerBasis[status: done; S-pairs encountered up to degree 1] (of class GroebnerBasis)

i100 : gb I2

o100 = GroebnerBasis[status: done; S-pairs encountered up to degree 1]

o100 : GroebnerBasis

i101 : GroebnerBasis _* := G -> flatten entries gens G

o101 = {*Function[stdio:92:23-92:47]*}

o101 : FunctionClosure

i102 : (gb I2)_*

o102 = {x}

o102 : List

i103 : new VerticalList from {a,b,c}

o103 = {a}
       {b}
       {c}

o103 : VerticalList

i104 : [a,b,c]

o104 = [a, b, c]

o104 : Array

i105 : {a,b,c}

o105 = {a, b, c}

o105 : List

i106 : (a,b,c)

o106 = (a, b, c)

o106 : Sequence

i107 : methods List

o107 = {(%, List, Number)                           }
       {(%, List, RingElement)                      }
       {(*, Thing, List)                            }
       {(++, OptionTable, List)                     }
       {(+, List, List)                             }
       {(-, List)                                   }
       {(-, List, List)                             }
       {(-, List, Set)                              }
       {(-, Set, List)                              }
       {(.., List, List)                            }
       {(..<, List, List)                           }
       {(/, List, Command)                          }
       {(/, List, Function)                         }
       {(/, List, RingMap)                          }
       {(/, List, SelfInitializingType)             }
       {(/, List, Thing)                            }
       {(/, Module, List)                           }
       {(/, Ring, List)                             }
       {(//, List, Number)                          }
       {(//, List, RingElement)                     }
       {(<<, List, Thing)                           }
       {(>>, List, Function)                        }
       {(?, List, List)                             }
       {(\, RingMap, List)                          }
       {(^, Matrix, List)                           }
       {(^, Module, List)                           }
       {(^, MutableMatrix, List)                    }
       {(^, RawRing, List)                          }
       {(^, Ring, List)                             }
       {(^, SheafOfRings, List)                     }
       {(_, Ideal, List)                            }
       {(_, Matrix, List)                           }
       {(_, Module, List)                           }
       {(_, Monoid, List)                           }
       {(_, MutableMatrix, List)                    }
       {(_, PolynomialRing, List)                   }
       {(_, Ring, List)                             }
       {(_, VisibleList, List)                      }
       {({*Function*}, List)                        }
       {({*Function*}, List)                        }
       {({*Function*}, List)                        }
       {({*Function*}, List)                        }
       {({*Function*}, List, Function)              }
       {({*Function*}, Ring, List)                  }
       {(|, List, List)                             }
       {(ascii, List)                               }
       {(basis, InfiniteNumber, List, Ideal)        }
       {(basis, InfiniteNumber, List, Matrix)       }
       {(basis, InfiniteNumber, List, Module)       }
       {(basis, InfiniteNumber, List, Ring)         }
       {(basis, List, Ideal)                        }
       {(basis, List, InfiniteNumber, Ideal)        }
       {(basis, List, InfiniteNumber, Matrix)       }
       {(basis, List, InfiniteNumber, Module)       }
       {(basis, List, InfiniteNumber, Ring)         }
       {(basis, List, List, Ideal)                  }
       {(basis, List, List, Matrix)                 }
       {(basis, List, List, Module)                 }
       {(basis, List, List, Ring)                   }
       {(basis, List, Matrix)                       }
       {(basis, List, Module)                       }
       {(basis, List, Ring)                         }
       {(basis, List, ZZ, Ideal)                    }
       {(basis, List, ZZ, Ring)                     }
       {(basis, ZZ, List, Ideal)                    }
       {(basis, ZZ, List, Ring)                     }
       {(chainComplex, List)                        }
       {(code, List)                                }
       {(columnPermute, MutableMatrix, ZZ, List)    }
       {(columnPermute, RawMutableMatrix, ZZ, List) }
       {(commonRing, List)                          }
       {(degreesMonoid, List)                       }
       {(degreesRing, List)                         }
       {(diagonalMatrix, List)                      }
       {(diagonalMatrix, Ring, List)                }
       {(diagonalMatrix, Ring, ZZ, ZZ, List)        }
       {(diagonalMatrix, RingFamily, List)          }
       {(diagonalMatrix, RingFamily, ZZ, ZZ, List)  }
       {(diagonalMatrix, ZZ, ZZ, List)              }
       {(directSum, List)                           }
       {(document, List)                            }
       {(drop, BasicList, List)                     }
       {(dual, MonomialIdeal, List)                 }
       {(eliminate, Ideal, List)                    }
       {(eliminate, List, Ideal)                    }
       {(export, List)                              }
       {(exportFrom, Package, List)                 }
       {(exportMutable, List)                       }
       {(findFiles, List)                           }
       {(findHeft, List)                            }
       {(fourierMotzkinElimination, List, List, ZZ) }
       {(gcd, List)                                 }
       {(gcdLLL, List)                              }
       {(generateAssertions, List)                  }
       {(gradedModule, List)                        }
       {(gradedModuleMap, List)                     }
       {(hashTable, List)                           }
       {(help, List)                                }
       {(hilbertFunction, List, CoherentSheaf)      }
       {(hilbertFunction, List, Ideal)              }
       {(hilbertFunction, List, Module)             }
       {(hilbertFunction, List, ProjectiveVariety)  }
       {(hilbertFunction, List, Ring)               }
       {(homogenize, Matrix, RingElement, List)     }
       {(homogenize, Module, RingElement, List)     }
       {(homogenize, RingElement, RingElement, List)}
       {(homogenize, Vector, RingElement, List)     }
       {(hypertext, List)                           }
       {(ideal, List)                               }
       {(intersect, List)                           }
       {(isRedundant, List, Set)                    }
       {(lcm, List)                                 }
       {(lift, List, QQ[x], QQ)                     }
       {(lift, List, QQ[x], QQ[x])                  }
       {(lift, List, QQ[x], ZZ)                     }
       {(lift, List, QQFlint, QQFlint)              }
       {(lift, List, QQFlint, ZZFlint)              }
       {(lift, List, R, QQ)                         }
       {(lift, List, R, R)                          }
       {(lift, List, R, ZZ)                         }
       {(lift, List, ZZFlint, ZZFlint)              }
       {(listSymbols, List)                         }
       {(makePackageIndex, List)                    }
       {(map, Module, Module, List)                 }
       {(map, Module, Module, RingMap, List)        }
       {(map, Module, Nothing, List)                }
       {(map, Module, Nothing, RingMap, List)       }
       {(map, Module, ZZ, List)                     }
       {(map, Ring, Ring, List)                     }
       {(mathML, List)                              }
       {(matrix, List)                              }
       {(matrix, Ring, List)                        }
       {(matrix, RingFamily, List)                  }
       {(memoize, Function, List)                   }
       {(monoid, List)                              }
       {(monomialIdeal, List)                       }
       {(mutableMatrix, List)                       }
       {(net, List)                                 }
       {(NewFromMethod, BR, List)                   }
       {(NewFromMethod, DocumentTag, List)          }
       {(NewFromMethod, HashTable, List)            }
       {(NewFromMethod, HR, List)                   }
       {(NewFromMethod, HREF, List)                 }
       {(NewFromMethod, MarkUpType, List)           }
       {(NewFromMethod, Module, List)               }
       {(NewFromMethod, Set, List)                  }
       {(NewFromMethod, TO, List)                   }
       {(NewFromMethod, TO2, List)                  }
       {(NewFromMethod, TOH, List)                  }
       {(norm, List)                                }
       {(ofClass, List)                             }
       {(part, List, RingElement)                   }
       {(peek', ZZ, List)                           }
       {(precedence, List)                          }
       {(pretty2, List)                             }
       {(primitive, List)                           }
       {(product, List)                             }
       {(promote, List, CC , CC )                   }
       {                  *    *                    }
       {(promote, List, QQ, CC )                    }
       {                      *                     }
       {(promote, List, QQ, QQ)                     }
       {(promote, List, QQ, QQ[x])                  }
       {(promote, List, QQ, R)                      }
       {(promote, List, QQ, RR )                    }
       {                      *                     }
       {(promote, List, QQ[x], QQ[x])               }
       {(promote, List, QQFlint, QQFlint)           }
       {(promote, List, R, R)                       }
       {(promote, List, RR , CC )                   }
       {                  *    *                    }
       {(promote, List, RR , RR )                   }
       {                  *    *                    }
       {(promote, List, ZZ, CC )                    }
       {                      *                     }
       {(promote, List, ZZ, QQ)                     }
       {(promote, List, ZZ, QQ[x])                  }
       {(promote, List, ZZ, R)                      }
       {(promote, List, ZZ, RR )                    }
       {                      *                     }
       {(promote, List, ZZ, ZZ)                     }
       {(promote, List, ZZFlint, QQFlint)           }
       {(promote, List, ZZFlint, ZZFlint)           }
       {(random, List)                              }
       {(random, List, Ring)                        }
       {(rowPermute, MutableMatrix, ZZ, List)       }
       {(rowPermute, RawMutableMatrix, ZZ, List)    }
       {(rsort, List)                               }
       {(scanLines, Function, List)                 }
       {(searchPath, List, String)                  }
       {(selectVariables, List, PolynomialRing)     }
       {(sort, List)                                }
       {(SPACE, HeaderType, List)                   }
       {(SPACE, Ring, List)                         }
       {(SPACE, WrapperType, List)                  }
       {(standardPairs, MonomialIdeal, List)        }
       {(submatrixByDegrees, Matrix, List, List)    }
       {(subsets, List)                             }
       {(subsets, List, ZZ)                         }
       {(substitute, Ideal, List)                   }
       {(substitute, Matrix, List)                  }
       {(substitute, Module, List)                  }
       {(substitute, RingElement, List)             }
       {(substitute, Vector, List)                  }
       {(sum, List)                                 }
       {(SYNOPSIS, List)                            }
       {(take, BasicList, List)                     }
       {(TEST, List)                                }
       {(texMath, List)                             }
       {(toZZ, List)                                }
       {(transpose, List)                           }
       {(truncate, List, Ideal)                     }
       {(truncate, List, Module)                    }
       {(undocumented, List)                        }
       {(unique, List)                              }
       {(vars, List)                                }
       {(vector, List)                              }
       {(weightRange, List, RingElement)            }

o107 : VerticalList

i108 : Foo = new Type BasicList
stdio:99:11:(3): error: no method for adjacent objects:
--            Type (of class Type)
--    SPACE   BasicList (of class Type)

i109 : Foo = new Type of BasicList

o109 = Foo

o109 : Type

i110 : new Foo from {a,b,c}

o110 = Foo{a, b, c}

o110 : Foo

i111 : p = oo

o111 = Foo{a, b, c}

o111 : Foo

i112 : net p

o112 = Foo{a, b, c}

i113 : x

o113 = x

o113 : R

i114 : p = new Foo from {a,b,c, x^2}

                     2
o114 = Foo{a, b, c, x }

o114 : Foo

i115 : net Foo := y -> between_", " (toList y/net)

o115 = {*Function[stdio:106:14-106:40]*}

o115 : FunctionClosure

i116 : p

                              2
o116 = {a, , , b, , , c, , , x }

o116 : Foo

i117 : peek oo

                     2
o117 = Foo{a, b, c, x }

i118 : toExternalString ooo

o118 = new Foo from {a,b,c,x^2}

i119 : peek net p

                                         +--+
                                         | 2|
o119 = {"a", ", ", "b", ", ", "c", ", ", |x |}
                                         +--+

i120 : net p

                              2
o120 = {a, , , b, , , c, , , x }

o120 : List

i121 : horizontalJoin oo

                 2
o121 = a, b, c, x

i122 : net Foo := y -> horizontalJoin between_", " (toList y/net)

o122 = {*Function[stdio:113:14-113:55]*}

o122 : FunctionClosure

i123 : p

                 2
o123 = a, b, c, x

o123 : Foo

i124 : y->y

o124 = {*Function[stdio:115:2-115:4]*}

o124 : FunctionClosure

i125 : code oo

o125 = 

i126 : methods List

o126 = {(%, List, Number)                           }
       {(%, List, RingElement)                      }
       {(*, Thing, List)                            }
       {(++, OptionTable, List)                     }
       {(+, List, List)                             }
       {(-, List)                                   }
       {(-, List, List)                             }
       {(-, List, Set)                              }
       {(-, Set, List)                              }
       {(.., List, List)                            }
       {(..<, List, List)                           }
       {(/, List, Command)                          }
       {(/, List, Function)                         }
       {(/, List, RingMap)                          }
       {(/, List, SelfInitializingType)             }
       {(/, List, Thing)                            }
       {(/, Module, List)                           }
       {(/, Ring, List)                             }
       {(//, List, Number)                          }
       {(//, List, RingElement)                     }
       {(<<, List, Thing)                           }
       {(>>, List, Function)                        }
       {(?, List, List)                             }
       {(\, RingMap, List)                          }
       {(^, Matrix, List)                           }
       {(^, Module, List)                           }
       {(^, MutableMatrix, List)                    }
       {(^, RawRing, List)                          }
       {(^, Ring, List)                             }
       {(^, SheafOfRings, List)                     }
       {(_, Ideal, List)                            }
       {(_, Matrix, List)                           }
       {(_, Module, List)                           }
       {(_, Monoid, List)                           }
       {(_, MutableMatrix, List)                    }
       {(_, PolynomialRing, List)                   }
       {(_, Ring, List)                             }
       {(_, VisibleList, List)                      }
       {({*Function*}, List)                        }
       {({*Function*}, List)                        }
       {({*Function*}, List)                        }
       {({*Function*}, List)                        }
       {({*Function*}, List, Function)              }
       {({*Function*}, Ring, List)                  }
       {(|, List, List)                             }
       {(ascii, List)                               }
       {(basis, InfiniteNumber, List, Ideal)        }
       {(basis, InfiniteNumber, List, Matrix)       }
       {(basis, InfiniteNumber, List, Module)       }
       {(basis, InfiniteNumber, List, Ring)         }
       {(basis, List, Ideal)                        }
       {(basis, List, InfiniteNumber, Ideal)        }
       {(basis, List, InfiniteNumber, Matrix)       }
       {(basis, List, InfiniteNumber, Module)       }
       {(basis, List, InfiniteNumber, Ring)         }
       {(basis, List, List, Ideal)                  }
       {(basis, List, List, Matrix)                 }
       {(basis, List, List, Module)                 }
       {(basis, List, List, Ring)                   }
       {(basis, List, Matrix)                       }
       {(basis, List, Module)                       }
       {(basis, List, Ring)                         }
       {(basis, List, ZZ, Ideal)                    }
       {(basis, List, ZZ, Ring)                     }
       {(basis, ZZ, List, Ideal)                    }
       {(basis, ZZ, List, Ring)                     }
       {(chainComplex, List)                        }
       {(code, List)                                }
       {(columnPermute, MutableMatrix, ZZ, List)    }
       {(columnPermute, RawMutableMatrix, ZZ, List) }
       {(commonRing, List)                          }
       {(degreesMonoid, List)                       }
       {(degreesRing, List)                         }
       {(diagonalMatrix, List)                      }
       {(diagonalMatrix, Ring, List)                }
       {(diagonalMatrix, Ring, ZZ, ZZ, List)        }
       {(diagonalMatrix, RingFamily, List)          }
       {(diagonalMatrix, RingFamily, ZZ, ZZ, List)  }
       {(diagonalMatrix, ZZ, ZZ, List)              }
       {(directSum, List)                           }
       {(document, List)                            }
       {(drop, BasicList, List)                     }
       {(dual, MonomialIdeal, List)                 }
       {(eliminate, Ideal, List)                    }
       {(eliminate, List, Ideal)                    }
       {(export, List)                              }
       {(exportFrom, Package, List)                 }
       {(exportMutable, List)                       }
       {(findFiles, List)                           }
       {(findHeft, List)                            }
       {(fourierMotzkinElimination, List, List, ZZ) }
       {(gcd, List)                                 }
       {(gcdLLL, List)                              }
       {(generateAssertions, List)                  }
       {(gradedModule, List)                        }
       {(gradedModuleMap, List)                     }
       {(hashTable, List)                           }
       {(help, List)                                }
       {(hilbertFunction, List, CoherentSheaf)      }
       {(hilbertFunction, List, Ideal)              }
       {(hilbertFunction, List, Module)             }
       {(hilbertFunction, List, ProjectiveVariety)  }
       {(hilbertFunction, List, Ring)               }
       {(homogenize, Matrix, RingElement, List)     }
       {(homogenize, Module, RingElement, List)     }
       {(homogenize, RingElement, RingElement, List)}
       {(homogenize, Vector, RingElement, List)     }
       {(hypertext, List)                           }
       {(ideal, List)                               }
       {(intersect, List)                           }
       {(isRedundant, List, Set)                    }
       {(lcm, List)                                 }
       {(lift, List, QQ[x], QQ)                     }
       {(lift, List, QQ[x], QQ[x])                  }
       {(lift, List, QQ[x], ZZ)                     }
       {(lift, List, QQFlint, QQFlint)              }
       {(lift, List, QQFlint, ZZFlint)              }
       {(lift, List, R, QQ)                         }
       {(lift, List, R, R)                          }
       {(lift, List, R, ZZ)                         }
       {(lift, List, ZZFlint, ZZFlint)              }
       {(listSymbols, List)                         }
       {(makePackageIndex, List)                    }
       {(map, Module, Module, List)                 }
       {(map, Module, Module, RingMap, List)        }
       {(map, Module, Nothing, List)                }
       {(map, Module, Nothing, RingMap, List)       }
       {(map, Module, ZZ, List)                     }
       {(map, Ring, Ring, List)                     }
       {(mathML, List)                              }
       {(matrix, List)                              }
       {(matrix, Ring, List)                        }
       {(matrix, RingFamily, List)                  }
       {(memoize, Function, List)                   }
       {(monoid, List)                              }
       {(monomialIdeal, List)                       }
       {(mutableMatrix, List)                       }
       {(net, List)                                 }
       {(NewFromMethod, BR, List)                   }
       {(NewFromMethod, DocumentTag, List)          }
       {(NewFromMethod, HashTable, List)            }
       {(NewFromMethod, HR, List)                   }
       {(NewFromMethod, HREF, List)                 }
       {(NewFromMethod, MarkUpType, List)           }
       {(NewFromMethod, Module, List)               }
       {(NewFromMethod, Set, List)                  }
       {(NewFromMethod, TO, List)                   }
       {(NewFromMethod, TO2, List)                  }
       {(NewFromMethod, TOH, List)                  }
       {(norm, List)                                }
       {(ofClass, List)                             }
       {(part, List, RingElement)                   }
       {(peek', ZZ, List)                           }
       {(precedence, List)                          }
       {(pretty2, List)                             }
       {(primitive, List)                           }
       {(product, List)                             }
       {(promote, List, CC , CC )                   }
       {                  *    *                    }
       {(promote, List, QQ, CC )                    }
       {                      *                     }
       {(promote, List, QQ, QQ)                     }
       {(promote, List, QQ, QQ[x])                  }
       {(promote, List, QQ, R)                      }
       {(promote, List, QQ, RR )                    }
       {                      *                     }
       {(promote, List, QQ[x], QQ[x])               }
       {(promote, List, QQFlint, QQFlint)           }
       {(promote, List, R, R)                       }
       {(promote, List, RR , CC )                   }
       {                  *    *                    }
       {(promote, List, RR , RR )                   }
       {                  *    *                    }
       {(promote, List, ZZ, CC )                    }
       {                      *                     }
       {(promote, List, ZZ, QQ)                     }
       {(promote, List, ZZ, QQ[x])                  }
       {(promote, List, ZZ, R)                      }
       {(promote, List, ZZ, RR )                    }
       {                      *                     }
       {(promote, List, ZZ, ZZ)                     }
       {(promote, List, ZZFlint, QQFlint)           }
       {(promote, List, ZZFlint, ZZFlint)           }
       {(random, List)                              }
       {(random, List, Ring)                        }
       {(rowPermute, MutableMatrix, ZZ, List)       }
       {(rowPermute, RawMutableMatrix, ZZ, List)    }
       {(rsort, List)                               }
       {(scanLines, Function, List)                 }
       {(searchPath, List, String)                  }
       {(selectVariables, List, PolynomialRing)     }
       {(sort, List)                                }
       {(SPACE, HeaderType, List)                   }
       {(SPACE, Ring, List)                         }
       {(SPACE, WrapperType, List)                  }
       {(standardPairs, MonomialIdeal, List)        }
       {(submatrixByDegrees, Matrix, List, List)    }
       {(subsets, List)                             }
       {(subsets, List, ZZ)                         }
       {(substitute, Ideal, List)                   }
       {(substitute, Matrix, List)                  }
       {(substitute, Module, List)                  }
       {(substitute, RingElement, List)             }
       {(substitute, Vector, List)                  }
       {(sum, List)                                 }
       {(SYNOPSIS, List)                            }
       {(take, BasicList, List)                     }
       {(TEST, List)                                }
       {(texMath, List)                             }
       {(toZZ, List)                                }
       {(transpose, List)                           }
       {(truncate, List, Ideal)                     }
       {(truncate, List, Module)                    }
       {(undocumented, List)                        }
       {(unique, List)                              }
       {(vars, List)                                }
       {(vector, List)                              }
       {(weightRange, List, RingElement)            }

o126 : VerticalList

i127 : peek I

o127 = Ideal{cache => CacheTable{}}
             generators => | x |
             ring => QQ[x]

i128 : peek Ideal

o128 = Type of HashTable{(%, Matrix, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/classes.m2:60:48-60:55]*}                                                               }
                         (%, Number, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix2.m2:352:25-356:111]*}
                         (%, RingElement, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix2.m2:347:30-351:93]*}
                         (*, Ideal, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/classes.m2:60:48-60:55]*}
                         (*, Ideal, Module) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/classes.m2:60:48-60:55]*}
                         (*, Ideal, Ring) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:293:32-293:85]*}
                         (*, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:471:39-471:65]*}
                         (*, Ideal, Vector) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:3:25-4:32]*}
                         (*, Ideal, ZZ) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:473:21-473:46]*}
                         (*, Ring, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:294:32-294:85]*}
                         (*, RingElement, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:470:39-470:65]*}
                         (*, ZZ, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:472:21-472:46]*}
                         (+, Ideal, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/classes.m2:60:48-60:55]*}
                         (+, Ideal, Number) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/classes.m2:60:48-60:55]*}
                         (+, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/classes.m2:60:48-60:55]*}
                         (+, Number, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/classes.m2:60:48-60:55]*}
                         (+, RingElement, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/classes.m2:60:48-60:55]*}
                         (/, EngineRing, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/quotring.m2:188:45-236:6]*}
                         (/, Ideal, Function) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:477:35-477:74]*}
                         (/, Ideal, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:504:34-504:55]*}
                         (/, Module, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:505:35-505:47]*}
                         (/, Ring, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/quotring.m2:176:39-181:6]*}
                         (:, Ideal, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/colon.m2:162:33-162:47]*}
                         (:, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/colon.m2:163:39-163:53]*}
                         (:, Module, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/colon.m2:164:35-164:49]*}
                         (==, Ideal, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:547:25-553:40]*}
                         (==, Ideal, Module) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:556:26-556:41]*}
                         (==, Ideal, Ring) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:540:24-543:17]*}
                         (==, Ideal, ZZ) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:297:22-302:11]*}
                         (==, Module, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:557:26-557:41]*}
                         (==, Ring, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:545:24-545:32]*}
                         (==, ZZ, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:296:22-296:30]*}
                         (\, Function, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:478:35-478:74]*}
                         (^, Ideal, Array) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:595:25-607:25]*}
                         (^, Ideal, ZZ) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:512:30-512:67]*}
                         (_, Ideal, List) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:536:23-536:37]*}
                         (_, Ideal, ZZ) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:521:36-521:57]*}
                         ({*Function*}, Ring, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/newring.m2:219:35-219:40]*}
                         (analyticSpread, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:398:51-398:78]*}
                         (associatedGradedRing, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:219:48-219:73]*}
                         (basis, InfiniteNumber, InfiniteNumber, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:784:28-784:65]*}
                         (basis, InfiniteNumber, List, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:784:28-784:65]*}
                         (basis, InfiniteNumber, ZZ, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:784:28-784:65]*}
                         (basis, List, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:773:46-773:79]*}
                         (basis, List, InfiniteNumber, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:784:28-784:65]*}
                         (basis, List, List, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:784:28-784:65]*}
                         (basis, List, ZZ, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:784:28-784:65]*}
                         (basis, ZZ, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:773:46-773:79]*}
                         (basis, ZZ, InfiniteNumber, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:784:28-784:65]*}
                         (basis, ZZ, List, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:784:28-784:65]*}
                         (basis, ZZ, ZZ, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:784:28-784:65]*}
                         (coerce, Ideal, Ring) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/classes.m2:60:48-60:55]*}
                         (coerce, Ring, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/newring.m2:147:33-147:42]*}
                         (distinguished, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:427:47-433:39]*}
                         (distinguishedAndMult, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:491:54-500:60]*}
                         (eliminate, Ideal, List) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/Elimination.m2:95:34-95:49]*}
                         (eliminate, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/Elimination.m2:94:41-94:58]*}
                         (eliminate, List, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/Elimination.m2:83:34-91:26]*}
                         (eliminate, RingElement, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/Elimination.m2:96:40-96:57]*}
                         (endomorphisms, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/IntegralClosure.m2:533:43-545:9]*}
                         (Ext, Ideal, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/ext.m2:184:28-184:51]*}
                         (Ext, Ideal, Module) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/ext.m2:185:29-185:45]*}
                         (Ext, Ideal, Ring) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/ext.m2:183:27-183:45]*}
                         (Ext, Module, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/ext.m2:182:29-182:45]*}
                         (Ext, ZZ, Ideal, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/ext.m2:79:31-79:69]*}
                         (Ext, ZZ, Ideal, Matrix) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/ext.m2:73:32-73:63]*}
                         (Ext, ZZ, Ideal, Module) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/ext.m2:80:32-80:63]*}
                         (Ext, ZZ, Ideal, Ring) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/ext.m2:78:30-78:63]*}
                         (Ext, ZZ, Matrix, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/ext.m2:75:32-75:63]*}
                         (Ext, ZZ, Module, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/ext.m2:77:32-77:63]*}
                         (Fano, ZZ, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/fano.m2:60:35-68:12]*}
                         (Fano, ZZ, Ideal, Ring) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/fano.m2:9:42-57:10]*}
                         (getMinimalPoly, Ideal, RingElement, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/PrimaryDecomposition/radical.m2:37:58-44:9]*}
                         (greedySat, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/PrimaryDecomposition/GTZ.m2:133:39-149:13]*}
                         (hilbertFunction, List, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:188:70-195:22]*}
                         (hilbertFunction, ZZ, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:186:94-186:117]*}
                         (Hom, Ideal, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:483:38-483:62]*}
                         (Hom, Ideal, Module) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:484:39-484:56]*}
                         (Hom, Ideal, Ring) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:489:37-489:56]*}
                         (Hom, Module, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:485:39-485:56]*}
                         (Hom, Ring, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:490:37-490:56]*}
                         (homogenize, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix2.m2:428:49-428:79]*}
                         (idealizer, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/IntegralClosure.m2:503:37-528:67]*}
                         (installHilbertFunction, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/gb.m2:453:53-453:90]*}
                         (integralClosure, Ideal, ZZ) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/IntegralClosure.m2:892:36-908:21]*}
                         (isLinearType, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:199:42-205:24]*}
                         (isPrimary, Ideal, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/PrimaryDecomposition.m2:135:33-137:11]*}
                         (isReduction, Ideal, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:329:29-336:20]*}
                         (isReduction, Ideal, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:339:41-346:20]*}
                         (isSubset, Ideal, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:848:32-848:61]*}
                         (isSubset, Ideal, Module) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:850:33-850:55]*}
                         (isSubset, Module, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:849:33-849:55]*}
                         (leadTerm, ZZ, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:533:39-533:67]*}
                         (lift, Ideal, QQ) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:681:33-684:87]*}
                         (lift, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix2.m2:638:42-642:66]*}
                         (lift, Ideal, ZZ) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:681:33-684:87]*}
                         (localize, Ideal, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/PrimaryDecomposition/Eisenbud-Huneke-Vasconcelos.m2:184:38-193:31]*}
                         (multiplicity, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:240:48-245:27]*}
                         (normalCone, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:212:37-214:19]*}
                         (primaryComponent, Ideal, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/PrimaryDecomposition/Eisenbud-Huneke-Vasconcelos.m2:250:45-256:43]*}
                         (promote, Ideal, Number) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:457:37-457:68]*}
                         (promote, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:457:37-457:68]*}
                         (quotient, Ideal, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/colon.m2:157:48-157:83]*}
                         (quotient, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/colon.m2:158:48-158:89]*}
                         (quotient, Module, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/colon.m2:159:48-159:84]*}
                         (rad, Ideal, ZZ) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/PrimaryDecomposition/radical.m2:107:38-135:11]*}
                         (radical00, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/PrimaryDecomposition/radical.m2:61:39-77:6]*}
                         (reduceLinears, Ideal, Set) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/minPres.m2:291:31-321:28]*}
                         (reductionNumber, Ideal, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:570:40-584:6]*}
                         (reesAlgebra, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:178:38-178:71]*}
                         (reesIdeal, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:158:45-159:26]*}
                         (saturate, Ideal, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/colon.m2:288:40-356:31]*}
                         (saturate, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/colon.m2:359:47-359:78]*}
                         (saturate, Module, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/colon.m2:363:42-370:5]*}
                         (saturate, Vector, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/colon.m2:373:42-373:84]*}
                         (saturation, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/PrimaryDecomposition/GTZ.m2:158:40-158:55]*}
                         (specialFiber, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:314:37-321:21]*}
                         (specialFiberIdeal, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:292:42-294:51]*}
                         (substitute, Ideal, List) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/ringmap.m2:395:42-395:63]*}
                         (substitute, Ideal, Matrix) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/ringmap.m2:313:44-313:70]*}
                         (substitute, Ideal, Option) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/ringmap.m2:401:35-401:58]*}
                         (substitute, Ideal, Ring) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/ringmap.m2:317:42-317:61]*}
                         (substitute, Ideal, RingFamily) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/ringmap.m2:324:48-324:73]*}
                         (toDual, ZZ, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/InverseSystems.m2:112:24-122:10]*}
                         (Tor, ZZ, Ideal, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/tor.m2:50:31-50:69]*}
                         (Tor, ZZ, Ideal, Matrix) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/tor.m2:44:32-44:63]*}
                         (Tor, ZZ, Ideal, Module) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/tor.m2:51:32-51:63]*}
                         (Tor, ZZ, Ideal, Ring) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/tor.m2:49:30-49:63]*}
                         (Tor, ZZ, Matrix, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/tor.m2:46:32-46:63]*}
                         (Tor, ZZ, Module, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/tor.m2:48:32-48:63]*}
                         (truncate, List, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:829:42-829:71]*}
                         (truncate, ZZ, Ideal) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:831:40-831:58]*}
                         (vasconcelos, Ideal, RingElement) => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/IntegralClosure.m2:549:41-565:9]*}
                         _* => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:476:14-476:42]*}
                         {Standard, AfterNoPrint} => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:507:67-509:88]*}
                         {Standard, AfterPrint} => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:507:67-509:88]*}
                         analyticSpread => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:395:37-395:62]*}
                         annihilator => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:622:33-622:63]*}
                         associatedGradedRing => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:218:35-218:54]*}
                         associatedPrimes => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/PrimaryDecomposition/Eisenbud-Huneke-Vasconcelos.m2:69:37-69:52]*}
                         basis => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:802:21-802:44]*}
                         betti => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/chaincomplexes.m2:716:21-716:49]*}
                         codim => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/option.m2:6:20-8:34]*}
                         comodule => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:461:31-461:54]*}
                         containsDthPowers => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/InverseSystems.m2:98:29-108:5]*}
                         decompose => {*a cache function*}
                         degree => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:519:19-519:49]*}
                         degreeLength => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:453:25-453:46]*}
                         degrees => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:452:20-452:49]*}
                         degreesRing => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:454:24-454:44]*}
                         dim => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:515:16-515:43]*}
                         distinguished => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:418:35-424:39]*}
                         distinguishedAndMult => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:480:42-489:60]*}
                         engineMGBF4 => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/gb.m2:121:27-121:65]*}
                         euler => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:468:21-468:41]*}
                         eulers => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:467:22-467:43]*}
                         expression => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:444:25-444:122]*}
                         flattenRing => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/newring.m2:222:27-227:50]*}
                         gb => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/gb.m2:190:35-190:60]*}
                         gbBoolean => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/gb.m2:470:31-470:89]*}
                         gbRemove => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/gb.m2:462:42-464:74]*}
                         gbSnapshot => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/gb.m2:459:65-459:109]*}
                         genera => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:464:21-464:43]*}
                         generator => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:481:39-491:6]*}
                         generators => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:475:36-475:48]*}
                         genus => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:465:20-465:41]*}
                         groebnerBasis => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/gb.m2:130:29-130:65]*}
                         GTZ0 => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/PrimaryDecomposition/GTZ.m2:161:19-167:25]*}
                         GTZ1 => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/PrimaryDecomposition/GTZ.m2:170:19-183:6]*}
                         hilbertPolynomial => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:206:36-206:77]*}
                         hilbertSeries => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:137:29-137:64]*}
                         idealPrepare => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:589:25-589:29]*}
                         independentSets => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/monideal.m2:163:28-163:70]*}
                         integralClosure => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/IntegralClosure.m2:910:32-910:60]*}
                         integralClosures => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/IntegralClosure.m2:913:33-929:75]*}
                         inverseSystem => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/InverseSystems.m2:129:25-137:35]*}
                         irreducibleCharacteristicSeries => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/factor.m2:65:44-84:64]*}
                         isHomogeneous => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:450:28-450:56]*}
                         isIdeal => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:449:20-449:23]*}
                         isLinearType => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:190:25-196:24]*}
                         isMonomialIdeal => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/monideal.m2:196:30-196:136]*}
                         isPrimary => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/PrimaryDecomposition.m2:134:23-134:47]*}
                         isPrime => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/factor.m2:163:20-163:66]*}
                         jacobian => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:534:33-534:56]*}
                         leadTerm => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:532:33-532:59]*}
                         mathML => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/mathml.m2:174:19-179:77]*}
                         mingens => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:503:36-503:63]*}
                         minimalBetti => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/option.m2:6:20-8:34]*}
                         minimalPresentation => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/minPres.m2:412:59-414:23]*}
                         minimalPrimes => {*a cache function*}
                         minimalReduction => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:505:38-526:6]*}
                         module => {*a cache function*}
                         monomialIdeal => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/monideal.m2:180:46-180:77]*}
                         monomialSubideal => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/monideal.m2:309:31-322:34]*}
                         multidegree => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/modules2.m2:357:24-357:59]*}
                         multiplicity => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:233:32-238:27]*}
                         net => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:445:18-445:36]*}
                         newdecompose => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/PrimaryDecomposition/Shimoyama-Yokoyama.m2:98:36-98:53]*}
                         normalCone => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:208:24-210:19]*}
                         numgens => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:531:22-531:51]*}
                         parametersInIdeal => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/IntegralClosure.m2:936:30-961:13]*}
                         PD => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/PrimaryDecomposition/GTZ.m2:33:17-43:41]*}
                         poincare => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:535:23-535:44]*}
                         primaryDecomposition => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/PrimaryDecomposition.m2:114:41-130:32]*}
                         primaryDecompositionGTZ => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/PrimaryDecomposition/GTZ.m2:11:38-18:17]*}
                         prune => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/minPres.m2:412:59-414:23]*}
                         quotient => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:462:34-462:53]*}
                         rad => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/PrimaryDecomposition/radical.m2:81:22-104:11]*}
                         radical => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/radical.m2:147:35-161:11]*}
                         randomKRationalPoint => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/varieties.m2:451:33-481:6]*}
                         reesAlgebra => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:168:24-168:55]*}
                         reesIdeal => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:143:31-144:51]*}
                         regularity => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/chaincomplexes.m2:648:26-651:65]*}
                         removeLowestDimension => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/radical.m2:35:45-52:20]*}
                         resolution => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/res.m2:228:45-232:6]*}
                         ring => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:538:19-538:24]*}
                         saturate => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/colon.m2:361:33-361:73]*}
                         singularLocus => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/quotring.m2:308:45-308:71]*}
                         specialFiber => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:303:25-310:21]*}
                         specialFiberIdeal => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:286:30-288:51]*}
                         support => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix2.m2:381:22-381:95]*}
                         synonym => ideal
                         tangentCone => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/TangentCone.m2:20:29-36:11]*}
                         toExternalString => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:447:31-447:73]*}
                         topComponents => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/radical.m2:59:37-62:47]*}
                         toString => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:446:23-446:46]*}
                         trim => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/matrix1.m2:520:29-520:96]*}
                         universalEmbedding => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:108:45-111:24]*}
                         variety => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/varieties.m2:133:20-133:35]*}
                         whichGm => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/packages/ReesAlgebra.m2:588:20-610:19]*}
                         Wrap => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/last.m2:22:114-22:124]*}
                         ZZp => {*Function[../../../src/M2/M2-Macaulay2/M2/Macaulay2/m2/quotring.m2:56:19-92:11]*}

i129 : parent Ideal

o129 = HashTable

o129 : Type

i130 : ancestors Ideal

o130 = {Ideal, HashTable, Thing}

o130 : List

i131 : parent I

o131 = Nothing

o131 : Type
