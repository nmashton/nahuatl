--# -path=.:../abstract:../common:../../prelude

resource ResNah = open Prelude, Predef in {

flags coding=utf8;

param

  Number = Sg | Pl ;
  Person = P1 | P2 | P3 ;
  Animacy = Animate | Inanimate ;
  Specificity = Specific | Unspecific ;
  Possession = SpecificPossessor Person Number | UnspecificPossessor | Absolute ;

  Case = Absolutive | Locative ; -- | Vocative ;

  Transitivity = Itv | Tv | Dtv ; 
  PluralType = Glot | GlotRed | Tin | TinRed | Meh ;

  AgrS = SpecificS Person Number ;
  AgrDO = SpecificDO Person Number | UnspecificDO Animacy | ReflexiveDO ;
  AgrIO = SpecificIO Person Number | UnspecificIO Animacy | ReflexiveIO ;

  DirPrefix = NoDirection | Direction Dir ;
  Dir = Toward | Away ;

--  Reduplication = NoReduplication |
--                  Intensive |
--                  Distributive ;

  TAM = Present | Preterite Augment |
          Imperfect | Normative | Transitory Augment |
          Vetative | Future | Counterfactual | Optative |
          Directional Dir Status ;
  Augment = Augmented | NoAugment ;
  Status = Imperfective | Perfective | OptativeStatus ; -- named for similarly fusional Maya TAM morpheme

  ImpersonalS = UnspecificS Animacy ;
  PassiveS = AnimatePassiveS Person Number | InanimatePassiveS |
             UnspecificPassiveS Animacy ;

oper

-- Phrasal types.
  NP : Type = {
    animacy : Animacy ;
    number : Number ;
    person : Person ;
    specificity : Specificity ;
    s : Str ;
  } ;

  VP : Type = {
    v : AgrS => TAM => Str ;
    c : AgrS => Str ;
    nom : Bool ;
  } ;

-- Nominal types.

  Noun : Type = {
    animacy : Animacy ;
    s : AgrS => Possession => Case => Str ;
  } ;

  Adjective : Type = {
    s : AgrS => Possession => Case => Str ;
  } ;

  Det : Type = {
    number : Number ;
    pref : Number => Str ;
    post : Str ;
  } ;

-- Verb types.

  IrregVerb = AgrS => TAM => Str ;

  ActiveIV = AgrS => DirPrefix => TAM => Str ;
  ImpersonalIV = ImpersonalS => DirPrefix => TAM => Str ;
  ImpersonalDepIV = DirPrefix => TAM => Str ;

  ActiveTV = AgrS => AgrDO => DirPrefix => TAM => Str ;
  PassiveTV = PassiveS => DirPrefix => TAM => Str ;

  ActiveDTV = AgrS => AgrDO => AgrIO => DirPrefix => TAM => Str ;
  PassiveDTV = PassiveS => AgrDO => DirPrefix => TAM => Str ;

  IVerb : Type = {
  	active : ActiveIV ;
    impersonal : ImpersonalIV ;
    causative : ActiveTV ;
    causative_passive : PassiveTV ;
    applicative : ActiveTV ;
    applicative_passive : PassiveTV ;
    honorific : ActiveIV ;
  } ;

  TVerb : Type = {
    active : ActiveTV ;
    passive : PassiveTV ;
    causative : ActiveDTV ;
    causative_passive : PassiveDTV ;
    applicative : ActiveDTV ;
    applicative_passive : PassiveDTV ;
    honorific : ActiveTV ; 
  } ;

  DTVerb : Type = {
    active : ActiveDTV ;
    passive : PassiveDTV ;
    honorific : ActiveDTV ;
    -- this probably needs more work ...
  } ;

  DeponentIVerb : Type = {
    active : ActiveIV ;
    impersonal : ImpersonalIV ;
    causative : ActiveTV ;
    causative_passive : PassiveTV ;
    applicative : ActiveTV ;
    applicative_passive : PassiveTV ;
    honorific : ActiveIV ;
  } ;

  DeponentTVerb : Type = {
    active : ActiveTV ;
    passive : PassiveTV ;
    causative : ActiveDTV ;
    causative_passive : PassiveDTV ;
    honorific : ActiveTV ;
  } ;

--- Constructors for phrasal types.

  pronNP : (form : Str) -> Person -> Number -> NP =
    \str,p,n ->
    {
      animacy = Animate ;
      number = n ;
      person = p ;
      specificity = Specific ;
      s = str ;
    } ;

--- Constructors for nouns.

noun : (sg,pl,poss : Str) -> Animacy -> AgrS => Possession => Case => Str =
  \sg,pl,poss,anim ->
    \\s,p,c =>
      (fixFinal
        (epenthesis
          (subjectPrefix s Present
            (possPrefix p
              (nounStem sg pl poss anim s p c))))) ;

possPrefix : Possession -> Str -> Str =
-- possO handles the special behavior of the possessive
-- prefix's "-o" in context
  \p,stem ->
  case p of {
    SpecificPossessor P1 Sg => "n" + (possO stem) ;
    SpecificPossessor P1 Pl => "t" + (possO stem) ;
    SpecificPossessor P2 Sg => "m" + (possO stem) ;
    SpecificPossessor P2 Pl => "am" + (possO stem) ;
    SpecificPossessor P3 Sg => "ī" + stem ;
    SpecificPossessor P3 Pl => "ī" + (nasAssim stem) + stem ;
    UnspecificPossessor => "tē" + stem ;
    _ => stem
  } ;

nounStem : (sg,pl,poss : Str) -> Animacy -> AgrS -> Possession -> Case -> Str =
  \sg,pl,poss,anim,s,p,c ->
      case <anim,s,p,c> of {
        <Animate,_,_,Locative> |
          <Inanimate,SpecificS _ Pl,_,_> => nonExist ;
        <_,_,_,Locative> => (locative sg) ;
        <_,SpecificS _ Pl,Absolute,_> => pl ;
        <_,_,Absolute,_> => sg ;
        <_,SpecificS _ Pl,_,_> => (possPl poss) ;
        <_,_,_,_> => poss
      } ;

-- locative affixation
-- ... Predef.take doesn't want to play nice with variants, for whatever reason
locative : Str -> Str =
  \s ->
    case s of {
      a@(#cons | "") + b@(#vowel) + "tl" => a + b + "co" ; --("co" | "copa") ;
      x + "tl" => x + "c" ; --("c" | "cpa" | "copa") ;
      x + ("tli" | "li") => x + "co" ; --("co" | "copa") ;
      x + y@(#vowel) => x + y + "c" ; -- ("c" | "cpa" | "copa") ;
      x + y@(#cons) => x + y + "co" ; -- ("co" | "copa") ;
      _ => s
    } ;

-- smart noun pluralization
nounPlural : (sg : Str) -> PluralType -> Str =
  \sg,pt ->
    case pt of {
      Glot | GlotRed =>
        (glottalpl (case pt of {
            GlotRed => nounredup sg ;
            _ => sg })) ;
      Tin | TinRed =>
        (tin
          (case pt of {
            TinRed => nounredup sg ;
            _ => sg
          }));
      Meh =>
        (meh sg)
    } ;

-- helper functions for noun pluralization
nounredup : Str -> Str = \s ->
  case s of {
    c@(#cons) + v@(#vowel) + rest => c + (plLengthen v) + c + v + rest ;
    v@(#vowel | #glvowel) + rest => (plLengthen v) + v + rest ;
    _ => s 
  } ;
glottalpl : Str -> Str = \s ->
  case s of {
    te + o@(#vowel) + "tl" => te + (glottalize o) ;
    _ => s
  } ;
tin : Str -> Str = \s ->
  case s of {
    oquich + ("tli" | "in" | "li") => oquich + "tin" ;
    _ => s + "tin"
  } ;
meh : Str -> Str = \s ->
  case s of {
    mich + ("in" | "tli" | "li" | "tl") => mich + "mè" ;
    _ => s + "mè"  
  } ;

--- Irregular verbs.

ca_V : IrregVerb =
  \\s,t =>
    case t of {
      Present => "" ;
      _ => caStem s t
    } ;

caStem : AgrS -> TAM -> Str =
  \s,t ->
  let
    num = case s of {
      SpecificS _ n => n
    } ;
    stem = case <t,num> of {
        <Present,Sg> => "câ" ;
        <Present,Pl> => "catê" ;
        <Preterite _,Sg> => "catca" ;
        <Preterite _,Pl> => "catcâ" ;
        <Optative,Sg> => "ye" ;
        <Optative,Pl> => "yecān" ;
        <Future,Sg> => "yez" ;
        <Future,Pl> => "yezquê" ;
        <Imperfect,Sg> => "catca" ;
        <Imperfect,Pl> => "catcâ" ;
        <Normative,Sg> => "yeni" ;
        <Normative,Pl> => "yenî" ;
        <Transitory _,Sg> => "catca" ;
        <Transitory _,Pl> => "catcâ" ;
        <Counterfactual,Sg> => "yezquia" ;
        <Counterfactual,Pl> => "yezquiâ" ;
        <Vetative,Sg> => "ye" ;
        <Vetative,Pl> => "yecān" ;
        <Directional _ _,Sg> => "câ" ;
        <Directional _ _,Pl> => "catê" 
      } ;
  in
    (augment t) +
      (epenthesis
        (subjectPrefix s
          stem)) ;

yauh_V : IrregVerb =
  \\s,t => yauhStem s t ;

yauhStem : AgrS -> TAM -> Str =
  \s,t ->
  let
    num = case s of {
      SpecificS _ n => n 
    } ;
    stem = case <t,num> of {
        <Present,Sg> => "yauh" ;
        <Present,Pl> => "huî" ;
        <Preterite _,Sg> => "yâ" ;
        <Preterite _,Pl> => "yàquê" ;
        <Optative,Sg> => "yauh" ;
        <Optative,Pl> => "huiyān" ;
        <Future,Sg> => "yāz" ;
        <Future,Pl> => "yāzquê" ;
        <Imperfect,Sg> => "huiya" ;
        <Imperfect,Pl> => "huiyâ" ;
        <Normative,Sg> => "yāni" ;
        <Normative,Pl> => "yānî" ;
        <Transitory _,Sg> => "yauh" ;
        <Transitory _,Pl> => "huiyān" ;
        <Counterfactual,Sg> => "yāzquia" ;
        <Counterfactual,Pl> => "yāzquia" ;
        <Vetative,Sg> => "yauh" ;
        <Vetative,Pl> => "huiyān" ;
        <Directional _ _,Sg> => "yauh" ;
        <Directional _ _,Pl> => "huî" 
      } ;
  in
    (augment t) +
      (epenthesis
        (subjectPrefix s
          stem)) ;

huItz_V : IrregVerb =
  \\s,t => huItzStem s t ;

huItzStem : AgrS -> TAM -> Str =
  \s,t ->
  let
    num = case s of {
      SpecificS _ n => n
    } ;
    stem = case <t,num> of {
        <Present,Sg> => "huītz" ;
        <Present,Pl> => "huītzê" ;
        <Preterite _,Sg> => "huītza" ;
        <Preterite _,Pl> => "huītzâ" ;
        <Optative,Sg> => "huāllauh" ;
        <Optative,Pl> => "huālhuiyān" ;
        <Future,Sg> => "huāllāz" ;
        <Future,Pl> => "huāllāzquê" ;
        <Imperfect,Sg> => "huālhuiya" ;
        <Imperfect,Pl> => "huālhuiyâ" ;
        <Normative,Sg> => "huāllāni" ;
        <Normative,Pl> => "huāllānî" ;
        <Transitory _,Sg> => "huītza" ;
        <Transitory _,Pl> => "huītzâ" ;
        <Counterfactual,Sg> => "huāllāzquia" ;
        <Counterfactual,Pl> => "huāllāzquiâ" ;
        <Vetative,Sg> => "huāllauh" ;
        <Vetative,Pl> => "huālhuiyān" ;
        <Directional _ _,Sg> => "huītz" ;
        <Directional _ _,Pl> => "huītzê" 
      } ;
  in
    (augment t) +
      (epenthesis
        (subjectPrefix s
          stem)) ;


--- Constructors for intransitives.

ivActive : (b1,b2,b3 : Str) -> ActiveIV =
  \tEmoa,tEmo',tEmo ->
    \\s,d,t =>
      (augment t) + 
      (epenthesis
      (subjectPrefix s t 
      (directionPrefix d
      (subjectPlSuffix s t
      (tenseStem tEmoa tEmo' tEmo t))))) ;

ivImpersonal : (b1,b2,b3,impers : Str) -> ImpersonalIV =
  \b1,b2,b3,impers ->
    \\is,d,t =>
    let
      thestem = case is of {
          UnspecificS Inanimate => (tenseStem b1 b2 b3 t) ;
          _ => (tenseStem impers (base2 Itv impers) (base3 impers) t)
        } ;
    in
      (augment t) +
      (directionPrefix d
      (impersonalSubjPrefix is) +
      (subjectPlSuffix (SpecificS P3 Sg) t
      thestem)) ;

-- Constructors for transitives.

tvActive : (b1,b2,b3 : Str) -> ActiveTV =
  \tEmoa,tEmo',tEmo ->
    \\s,o,d,t =>
      (augment t) +
      (epenthesis
      (subjectPrefix s t
      (directObjectPrefix o
      (directionPrefix d
      (reflexivePrefix s o
      (indefiniteObjectPrefix o
      (subjectPlSuffix s t
      (tenseStem tEmoa tEmo' tEmo t)))))))) ;

tvPassive : (b1,b2,b3,b4 : Str) -> PassiveTV =
  \tEmoa,tEmo',tEmo,tEmOl ->
  let
    pS = passiveStem tEmOl ;
    pS2 = (base2 Itv pS) ;
    pS3 = (base3 pS) ;
  in
    \\s,d,t =>
      (augment t) +
      case s of {
        InanimatePassiveS =>
          (directionPrefix d
          (reflexivePrefix (SpecificS P3 Sg) ReflexiveDO
          (tenseStem tEmoa tEmo' tEmo t))) ;
        AnimatePassiveS pers num =>
          (epenthesis
          (subjectPrefix (SpecificS pers num)
          (directionPrefix d
          (subjectPlSuffix (SpecificS pers num) t
          (tenseStem pS pS2 pS3 t))))) ;
        UnspecificPassiveS anim =>
          (directionPrefix d
          (indefiniteObjectPrefix (UnspecificDO anim)
          (tenseStem pS pS2 pS3 t)))
      } ;

tvActiveDerived : (b1,b2,b3,b4 : Str) -> ActiveDTV =
  \tEmoa,tEmo',tEmo,tEmOl ->
    \\s,o,i,d,t =>
      (augment t) +
      (epenthesis
      (subjectPrefix s t
      (directObjectPrefix o i
      (directionPrefix d
      (reflexivePrefix s o
      (indefiniteObjectPrefix o i
      (innerReflexive i
      (subjectPlSuffix s t
      (tenseStem tEmoa tEmo' tEmo t))))))))) ;

-- Constructors for intransitive deponents.

ivDeponentActive : (b1,b2,b3 : Str) -> ActiveIV =
  \tEmoa,tEmo',tEmo ->
    \\s,d,t =>
      (augment t) +
      (epenthesis
      (subjectPrefix s t
      (directionPrefix d
      (reflexivePrefix s
      (subjectPlSuffix s t
      (tenseStem tEmoa tEmo' tEmo t)))))) ;

ivDeponentImpersonal : (b1,b2,b3,b4 : Str) -> ImpersonalDepIV =
  \tEmoa,tEmo',tEmo,tEmOl ->
  let
    pS = (passiveStem tEmOl) ;
    pS2 = (base2 Tv pS) ;
    pS3 = (base3 pS) ;
  in
    \\d,t =>
      (augment t) +
      (directionPrefix d
      (innerReflexive ReflexiveDO
      (tenseStem pS pS2 pS3 t))) ;

-- Constructors for ditransitives.

dtvActive : (b1,b2,b3,b4 : Str) -> ActiveDTV =
 \b1,b2,b3,b3 ->
  \\s,o,i,d,t =>
    (augment t) +
    (epenthesis
    (subjectPrefix s t
    (directObjectPrefix o i
    (directionPrefix d
    (reflexivePrefix s o i
    (indefiniteObjectPrefix o i
    (subjectPlSuffix s t
    (tenseStem b1 b2 b3 t)))))))) ;

dtvPassive : (b1,b2,b3,b4 : Str) -> PassiveDTV =
  \tEmoa,tEmo',tEmo,tEmOl ->
    \\s,o,d,t =>
      let
        pS = (passiveStem tEmOl) ;
        pS2 = (base2 Itv pS) ;
        pS3 = (base3 pS) ;
      in
        (augment t) +
        case s of {
          InanimatePassiveS =>
            (directionPrefix d
            (reflexivePrefix (SpecificS P3 Sg) ReflexiveDO
            ("tē" +
            (tenseStem tEmoa tEmo' tEmo t)))) ;
          AnimatePassiveS pers num =>
            let
              stem = (subjectPlSuffix (SpecificS pers num) t (tenseStem pS pS2 pS3 t)) ;
            in
              (epenthesis
              (subjectPrefix (SpecificS pers num)
              (directionPrefix d
              (innerReflexive o
              (case o of {UnspecificDO Inanimate => indefiniteObjectPrefix o stem ; _ => stem}))))) ;
          UnspecificPassiveS anim =>
            (directionPrefix d
            (innerReflexive o
            (indefiniteObjectPrefix o (UnspecificIO anim)
            (tenseStem pS pS2 pS3 t))))
        } ;

dtvHonorific : (b1,b2,b3,b4 : Str) -> ActiveDTV =
  \b1,b2,b3,b4 ->
    \\s,o,i,d,t =>
      (augment t) +
      (epenthesis
      (subjectPrefix s t
      (directObjectPrefix o i
      (directionPrefix d
      (reflexivePrefix s ReflexiveDO
      (indefiniteObjectPrefix o i
      (subjectPlSuffix s t
      (tenseStem b1 b2 b3 t)))))))) ;

-- Constructors for transitive deponents.

tvDeponentActive : (b1,b2,b3 : Str) -> ActiveTV =
  \tEmoa,tEmo',tEmo ->
    \\s,o,d,t =>
      (augment t) +
      (epenthesis
      (subjectPrefix s t
      (directObjectPrefix o
      (directionPrefix d
      (reflexivePrefix s
      (indefiniteObjectPrefix o     
      (subjectPlSuffix s t
      (tenseStem tEmoa tEmo' tEmo t)))))))) ;

tvDeponentPassive : (b1,b2,b3,b4 : Str) -> PassiveTV =
  \tEmoa,tEmo',tEmo,tEmOl ->
  let pS = (passiveStem tEmOl) in
    \\s,d,t =>
      (augment t) +
      case s of {
        InanimatePassiveS =>
          (directionPrefix d
          (reflexivePrefix (SpecificS P3 Sg) ReflexiveDO
          ("tē" +
          (tenseStem tEmoa tEmo' tEmo t)))) ;
        AnimatePassiveS pers num =>
          (epenthesis
          (subjectPrefix (SpecificS pers num)
          (directionPrefix d
          (innerReflexive ReflexiveDO
          (subjectPlSuffix (SpecificS pers num) t
          (tenseStem pS pS pS t)))))) ;
        UnspecificPassiveS anim =>
          (directionPrefix d
          (innerReflexive ReflexiveDO
          (indefiniteObjectPrefix (UnspecificDO anim)
          (tenseStem pS pS pS t))))
      } ;

-- for applicatives and their weird reflexives
tvDeponentDerived : (b1,b2,b3 : Str) -> AgrS => AgrDO => DirPrefix => TAM => Str =
  \tEmoa,tEmo',tEmo ->
    \\s,o,d,t =>
      (augment t) +
      (epenthesis
      (subjectPrefix s t
      (directObjectPrefix o
      (directionPrefix d
      (innerReflexive ReflexiveDO
      (indefiniteObjectPrefix o     
      (subjectPlSuffix s t
      (tenseStem tEmoa tEmo' tEmo t)))))))) ;

-- Prefix-block morphology.

augment : TAM -> Str =
  \t -> case t of {
    (Preterite Augmented | Transitory Augmented) => "ō" ;
    _ => ""
  } ;

subjectPrefix = overload {
  subjectPrefix : AgrS -> Str -> Str= defaultSubjectPrefix ;
  subjectPrefix : AgrS -> TAM -> Str -> Str =
    \s,t,stem ->
      case <t,s> of {
        <Optative,SpecificS P2 _> => "x" + stem ;
        _ => defaultSubjectPrefix s stem
      } ;
} ;

defaultSubjectPrefix : AgrS -> Str -> Str =
  \s,stem ->
  let
    context = Predef.take 1 stem ;
    prefix =
      case s of {
        SpecificS P1 Sg => "n" ;
        SpecificS P1 Pl => "t" ;
        SpecificS P2 Sg => "t" ;
        SpecificS P2 Pl => secondPlS context ;
        SpecificS P3 _ => ""
      } ;
  in
    prefix + stem ;

reflexivePrefix = overload {
  reflexivePrefix : AgrS -> Str -> Str = defaultReflexivePrefix ;
  reflexivePrefix : AgrS -> AgrDO -> Str -> Str =
    \s,d,stem -> case d of {
      ReflexiveDO => defaultReflexivePrefix s stem ;
      _ => stem
    } ;
  reflexivePrefix : AgrS -> AgrDO -> AgrIO -> Str -> Str =
    \s,d,i,stem ->
    case <i,d> of {
      <ReflexiveIO,_> | <_,ReflexiveDO> => defaultReflexivePrefix s stem ;
      _ => stem
    } ;
} ;

defaultReflexivePrefix : AgrS -> Str -> Str = 
  \s,stem ->
  let
    pref = case s of {
      SpecificS P1 Sg => "n" ;
      SpecificS P1 Pl => "t" ;
      _ => "m"
    } ;
    o = reflexiveO stem ;
  in
    pref + o + stem ;

innerReflexive = overload {
  innerReflexive : AgrIO -> Str -> Str =
      \i,stem -> case i of {
        ReflexiveIO => defaultInnerReflexive stem ;
        _ => stem
      } ;
  innerReflexive : AgrDO -> Str -> Str =
      \o,stem -> case o of {
        ReflexiveDO => defaultInnerReflexive stem ;
        _ => stem
      } ;
} ;

defaultInnerReflexive : Str -> Str =
  \stem ->
    case stem of {
      #glvowel + tOtia => "nè" + tOtia ;
      e@("e" | "ē") + hua => "n" + e + hua ;
      _ => "ne" + stem 
    } ;

directObjectPrefix = overload {
  directObjectPrefix : AgrDO -> Str -> Str = defaultDirectObjectPrefix ;
  directObjectPrefix : AgrDO -> AgrIO -> Str -> Str = ditransitiveDirectObjectPrefix ;
} ;

ditransitiveDirectObjectPrefix : AgrDO -> AgrIO -> Str -> Str =
  \o,i,stem ->
    case <i,o> of {
      <ReflexiveIO,_> |
        <SpecificIO P3 Sg,_> |
        <SpecificIO P3 Pl,SpecificDO P3 Pl> => defaultDirectObjectPrefix o stem ;
      <SpecificIO P3 Pl,UnspecificDO _> => defaultDirectObjectPrefix (SpecificDO P3 Pl) stem ;
      <SpecificIO P3 Pl,_> => (defaultDirectObjectPrefix o ((thirdPlO stem) + stem)) ;
      <SpecificIO pers num,_> =>
        defaultDirectObjectPrefix
          (SpecificDO pers num)
          (case o of {
              SpecificDO P3 Pl => (thirdPlO stem) ;
              _ => ""
            } + stem) ;
      _ => defaultDirectObjectPrefix o stem
    } ;

--ditransitiveDirectObjectPrefix : AgrDO -> AgrIO -> Str -> Str =
--  \o,i,stem -> case i of {
--    ReflexiveIO | SpecificIO P3 Sg => defaultDirectObjectPrefix o stem ;
--    SpecificIO P3 Pl =>
--      case o of {
--        UnspecificDO _ => defaultDirectObjectPrefix (SpecificDO P3 Pl) stem ;
--        SpecificDO P3 Pl => defaultDirectObjectPrefix o stem ;
--        _ => (defaultDirectObjectPrefix o ((thirdPlO stem) + stem))
--      } ;
--    SpecificIO pers num =>
--      defaultDirectObjectPrefix
--        (SpecificDO pers num)
--        (case o of {
--            SpecificDO P3 Pl => (thirdPlO stem) ;
--            _ => ""
--          } + stem) ;
--    _ => (defaultDirectObjectPrefix o stem) 
--  } ;

defaultDirectObjectPrefix : AgrDO -> Str -> Str =
  \o, stem ->
  let
    context = Predef.take 1 stem ;
    pref = case o of {
      SpecificDO P1 Sg => "nēch" ;
      SpecificDO P1 Pl => "tēch" ;
      SpecificDO P2 Sg => "mitz" ;
      SpecificDO P2 Pl => "amēch" ;
      SpecificDO P3 Sg => object3sg context ;
      SpecificDO P3 Pl => "qu" + (thirdPlO context) ;
      _ => "" 
    } ;
  in
    pref + stem ;

  thirdPlO : Str -> Str = \s ->
  let
    f = Predef.take 1 s ;
  in
    case f of {
      "p" | "m" | #vowel | #glvowel => "im" ;
      _ => "in"
    } ;

indefiniteObjectPrefix = overload {
  indefiniteObjectPrefix : AgrDO -> Str -> Str =
    \o,stem -> case o of {
      UnspecificDO Animate => "tē" + stem ;
      UnspecificDO Inanimate => (tla stem) ;
      _ => stem
    } ;
  indefiniteObjectPrefix : AgrDO -> AgrIO -> Str -> Str =
    \d,i,stem -> case d of {
      (UnspecificDO Inanimate) =>
        case i of {
          (UnspecificIO Animate) => "tē" + (tla stem) ;
          (UnspecificIO Inanimate) => (tla stem) ;
          _ => (tla stem)
        } ;
      (UnspecificDO Animate) =>
        case i of {
          (UnspecificIO _) => "tē" + stem ;
          _ => "tē" + stem
        } ;
      _ =>
        case i of {
          (UnspecificIO Animate) => "tē" + stem ;
          (UnspecificIO Inanimate) => (tla stem) ;
          _ => stem
        } 
    } ;
} ;

tla : Str -> Str = \s ->
  case s of {
    "i" + tta => "tla" + tta ;
    "ì" + toa => "tlà" + toa ;
    _ => "tla" + s
  } ;

impersonalSubjPrefix : ImpersonalS -> Str =
  \is -> case is of {
    UnspecificS Inanimate => "tla" ;
    _ => ""
  } ;

directionPrefix : DirPrefix -> Str -> Str =
  \d,stem ->
    case d of {
      Direction Toward => "huāl" + (case stem of {"y" + x => "l" + x ; _ => stem }) ;
      Direction Away => "on" + stem ;
      _ => stem
    } ;

-- Stem morphology.

tenseStem : (b1,b2,b3 : Str) -> TAM -> Str =
  \tEmoa,tEmo',tEmo,tam ->
    case tam of {
      Present => tEmoa ;
      Preterite _ => (preteriteStem tEmo') ;
      Imperfect => (stemLengthening tEmoa) + "ya" ;
      Normative => (stemLengthening tEmoa) + "ni" ;
      Transitory _ => tEmo' + "ca" ;
      Vetative => (vetativeStem tEmo') ;
      Future => (shortStemLengthening tEmo) + "z" ;
      Counterfactual => (shortStemLengthening tEmo) + "zquia" ;
      Optative => tEmo ;
      Directional _ _ => tEmo +
        case tam of {
          Directional Toward Imperfective => "quiuh" ;
          Directional Toward Perfective => "co" ;
          Directional Toward OptativeStatus => "qui" ;
          Directional Away Imperfective => "tīuh" ;
          Directional Away Perfective => "to" ;
          Directional Away OptativeStatus => "ti" ;
          _ => ""
        }        
    } ;

subjectPlSuffix : AgrS -> TAM -> Str -> Str =
  \s,t,stem ->
  let
    fixed = fixFinal stem ;
  in
    case s of {
      SpecificS _ Pl =>
        case t of {
          Vetative => vetativeSubjPl stem ;
          Future => stem + "quê" ;
          Preterite _ => preteriteSubjPl stem ;
          Optative => (shortStemLengthening stem) +  "cān" ;
          Directional _ _ => directionalSubjPl stem ;
          _ => (glottalSubjPl stem)
        } ;
      _ => fixed
    } ;

-- Helper functions for phrasal combination.

  handleNumber : Number -> Animacy -> Number =
    \n,a ->
      case <n,a> of {
        <Pl,Animate> => Pl ;
        _ => Sg
      } ;

  conjAnimacy : Animacy -> Animacy -> Animacy =
    \a,b ->
      case <a,b> of {<Animate,Animate> => Animate; _ => Inanimate} ;

  conjNumber : Number -> Number -> Number =
    \m,n ->
      case <m,n> of {<Pl,Pl> => Pl; _ => Sg} ; 

  conjPerson : Person -> Person -> Person =
    \p,q ->
      case <p,q> of {
        <P1,_> | <_,P1> => P1 ;
        <P2,_> | <_,P2> => P2 ;
        _               => P3
      } ;

  conjSpecificity : Specificity -> Specificity -> Specificity =
    \s,t ->
      case <s,t> of {<Specific,_> | <_,Specific> => Specific; _ => Unspecific} ;

-- The phonology.

  vowel : pattern Str =
    #("a" | "ā" | "e" | "ē" | "o" | "ō" | "i" | "ī" |
      "á" | "é" | "í" | "ó" | "ú") ; -- non-native

  glvowel : pattern Str =
    #("à" | "â" | "è" | "ê" | "ì" | "î" | "ò" | "ô") ;

  frontvowel : pattern Str =
    #("e" | "ē" | "è" | "ê" |
      "i" | "ī" | "ì" | "î") ;

  longvowel : pattern Str =
    #("ā" | "ē" | "ī" | "ō") ;

  cons : pattern Str =
    #("p" | "t" | "qu" | "c" | "tz" | "ch" | "tl" | "cu" |
      "uc" | "m" | "n" | "s" | "z" | "x" | "y" | "hu" | "uh" | "l" |
      "b" | "d" | "f" | "g" | "h" | "j" | "k" | "r" | "v" ) ; -- non-native

  nonlab : pattern Str =
    #("t" | "qu" | "q" | "c" | "tz" | "ch" | "tl" | "cu" |
      "uc" | "n" | "s" | "z" | "x" | "y" | "hu" | "h" | "uh" | "l" ) ;

  dicons : pattern Str =
    #("qu" | "tz" | "ch" | "tl" | "cu" | "uc" | "hu" | "uh") ;

-- rules for possessed noun forms
  possStem : Animacy -> Str -> Str = \a,s ->
    case s of {
      v@(#vowel) + "tl" => v + "uh" ;
      vc@((#vowel + #cons) | #glvowel) + "tli" => vc + "hui" ;
      vccv@(#vowel + #cons + #cons + #vowel) + "tl" => vccv + "uh" ;
      vcv@(#vowel + #cons + #vowel) + "tl" => vcv + "uh" ;
      pref + vc@(#vowel + ("qui" | "ti" | "chi")) + "tl" => pref + vc + "uh" ;

      pref + v@(#vowel) + cv@("ca" | "qui") + "tl" =>
        case a of {
          Animate => pref + v + cv + "uh" ;
          Inanimate => pref + v + "c"
        } ;
      pref + c@(#cons) + cv@("ca" | "qui") + "tl" =>
        case a of {
          Animate => pref + c + cv + "uh" ;
          Inanimate => pref + c + "qui"
        } ;

      pref + v@(#vowel) + cv@("hua" | "hui") + "tl" =>
        case a of {
          Animate => pref + v + cv + "uh" ;
          Inanimate => pref + v + "uh"
        } ;
      pref + c@(#cons) + cv@("hua" | "hui") + "tl" =>
        case a of {
          Animate => pref + c + cv + "uh" ;
          Inanimate => pref + c + "hui"
        } ;

      pref + v@(#vowel) + cv@("ma" | "mi") + "tl" =>
        case a of {
          Animate => pref + v + cv + "uh" ;
          Inanimate => pref + v + "n"
        } ;
      pref + c@(#cons) + cv@("ma" | "mi") + "tl" =>
        case a of {
          Animate => pref + c + cv + "uh" ;
          Inanimate => pref + c + "mi"
        } ;

      pref + v@(#vowel) + c@(#cons) + v2@("a" | "i") + "tl" =>
        case a of {
          Animate => pref + v + c + v2 + "uh" ;
          Inanimate => pref + v + c
        } ;
      pref + c@(#cons) + c2@(#cons) + v2@("a" | "i") + "tl" =>
        case a of {
          Animate => pref + c + c2 + v2 + "uh" ;
          Inanimate => pref + c + c2 + "i"
        } ;

      cu + v@("ā" | "ē") + "itl" => cu + v ;
      cal + ("li" | "in" | "tli") => cal ;
      te + "tl" => te + "uh" ;
      chich + i@(#vowel) => chich + i + "uh"
    } ;

  possPl : Str -> Str = \s ->
    case s of {
      chichi + "uh" => chichi + "huān" ;
      _ => s + "huān" 
    } ;

-- the stem formation rules
  base2 : Transitivity -> Str -> Str = \t,s ->
    case s of {
      _ + #vowel + "tla" => s ;
      _ + #vowel + "ca" => s ;
      e + "hua" =>
        case t of {
          Itv => e + "hua" ;
          _ => e + "uh"
        } ;
      xi + "m" + ("a" | "i") => xi + "n" ;
      huEyi + "ya" =>
        case t of {
          Itv => (s | huEyi + "x") ; -- Launey p. 73
          _ => huEyi + "x"
        }  ;
      aa + "yi" => aa + "x" ;
      i't + "oa" => i't + "ò" ;
      ilp + "ia" => ilp + "ì" ;
      pan + "o" => pan + "ō" ;
      m + i@(#vowel) + "qui" => m + i + "c" ;
      c + o@(#vowel) + ch@(#cons) + ("i" | "a") => c + o + ch ;
      _ => s
    } ;

  base3 : Str -> Str = \s ->
    case s of {
      tlAl + "ia" => tlAl + "i" ;
      i't + "oa" => i't + "o" ;
      _ => s 
    } ;

  base4 : Str -> Str = \s ->
    case s of {
      te + ("qui" | "ca") => te + "c" ;
      palEhu + "ia" => palEhu + "īl" ;
      i't + "oa" => i't + "ōl" ;
      aa + ("na" | "ni") => (aa + "n" | s + "l") ;
      tlA + ("ci" | "za") => ( tlA + "x" | s + "l") ;
      _ => (base3 s) + "l"
    } ;

  causativeStem : Str -> Str = \s ->
    case s of {
      chol + "oa" => chol + "ōl" ;
      mi + ("qui" | "ca") => mi + "c" ;
      m + a@(#longvowel) + ("hui" | "hua") => m + a + "uh" ;
      tlAhu + a@(#longvowel) + ("n" | "m") + #vowel => tlAhu + a + "n" ;
      nE + ("ci" | "za") => nE + "x" ;
      ilO + "ti" => ilO + "ch" ;
      yOl + "i" => yOl + "ī" ;
      quE + "m" => quE + "n" ;
      _ => s 
    } + "tia" ;

  applicativeStem : Str -> Str = \s ->
    case s of {
      cu@(#cons | "") + a@(#vowel) => cu + (lengthen a) ;
      palEhu + "ia" => palEhu + "ī" ;
      i't + "oa" => i't + "ō" ;
      p + "iya" => s ;
      nO + ("tza" | "tzi" | "ti" ) => nO + "chi" ;
      tlA + ("za" | "ci") => tlA + "xi" ; 
      tlazo' + "tla" => tlazo' + "chi" ;
      cuep + "a" => cuep + "i" ;
      _ => s
    } + "lia" ; 

  honorificStem : Str -> Str = \s ->
    case s of {
      "nāmiqui" => (applicativeStem s) ;
      ne + "qui" => ne + "quiltia" ;
      ma + "ti" => ma + "chiltia" ;
      _ => (applicativeStem s)
    } ;

  impersonalStem : Str -> Str = \s ->
    case s of {
      chO + ("ca" | "qui") => chO + "cōhua" ;
      ne + ("mi" | "ma") => ne + "mōhua" ;
      teo'ci + "hu" + ("i" | "a") => teo'ci + "ōhua" ;
      tza + g@(#cons|#glvowel) + "tzi" => tza + g + "tzīhua" ;
      a + g@(#cons|#glvowel) + "ci" => a + g + "xīhua" ;
      nE + ("ci" | "za") => nE + "xōhua" ;
      hue + "tz" + ("i" | "a") => hue + "chōhua" ;
      coch + v@(#vowel) => coch + (lengthen v) + "hua" ;
      _ => s 
    } ;

  passiveStem : Str -> Str = \s ->
    case s of {
      palEhui + l@(#cons | #glvowel) => palEhui + l + "o" ;
      quEm + i@(#vowel) => quEm + i + "hua" ;
      _ => s
    } ;

  preteriteStem : Str -> Str = \base2 ->
    case base2 of {
      tEmo + a@(#vowel) => tEmo + a + "c" ;
      _ => base2
    } ;

  preteriteSubjPl : Str -> Str = \s ->
    case s of {
      panO + "c" => panO + "quê" ;
      _ => s + "quê" 
    } ;

  vetativeStem : Str -> Str = \base2 ->
    case base2 of {
      tEmo + a@(#vowel) => tEmo + (glottalize a) ;
      _ => base2
    } ;

  vetativeSubjPl : Str -> Str = \s ->
    case s of {
      chOc + a'@(#glvowel) => chOc + (deglottalize a') + "tin" ;
      _ => s + "tin" 
    } ;

  directionalSubjPl : Str -> Str = \s ->
    case s of {
      miquiqui + "uh" => (glottalSubjPl (miquiqui + "hui")) ;
      _ => glottalSubjPl s
    } ;

  glottalSubjPl : Str -> Str = \s ->
    case s of {
      tEmo + a@(#vowel) => tEmo + (finGlottalize a) ;
      _ => s
    } ;

  reflexiveO : Str -> Str = \s ->
    case s of {
      v@(#glvowel | #vowel) + rest => "" ;
      _ => "o"
    } ;

  possO : Str -> Str = \s ->
  let
    f = Predef.take 1 s ;
    rest = Predef.drop 1 s ;
  in
    case f of {
      "i" => "o" + rest ;
      "ì" => "ò" + rest ;
      v@(#vowel) => s ;
      _ => "o" + s
    } ;

  secondPlS : Str -> Str = \s ->
  let
    f = Predef.take 1 s ;
  in
    case f of {
      #nonlab => "an" ;
      _ => "am" 
    } ;

  glottalize : Str -> Str = \vowel ->
    case vowel of {
      "a" | "ā" | "â" => "à" ;
      "e" | "ē" | "ê" => "è" ;
      "i" | "ī" | "î" => "ì" ;
      "o" | "ō" | "ô" => "ò" ;
      _ => vowel
    };

  finGlottalize : Str -> Str = \vowel ->
    case vowel of {
      "a" | "ā" => "â" ;
      "e" | "ē" => "ê" ;
      "i" | "ī" => "î" ;
      "o" | "ō" => "ô" ;
      _ => vowel
    };

  deglottalize : Str -> Str = \vowel ->
    case vowel of {
      "à" => "a" ;
      "è" => "e" ;
      "ì" => "i" ;
      "ò" => "o" ;
      _ => vowel
    } ;

  stemLengthening : Str -> Str = \s ->
    case s of { 
      a@(#cons | "") + b@(#vowel) => a + (lengthen b) ;
      a + b@("i" | "o") + c@(#vowel) => a + b + (lengthen c) ;
      a + "o" => a + "ō" ;
      _ => s
    } ;

  shortStemLengthening : Str -> Str = \s ->
    case s of {
      a + b@("i" | "o") => a + (lengthen b) ;
      a@(#cons | "") + b@(#vowel) => a + (lengthen b) ;
      _ => s 
    } ;

  fixFinal : Str -> Str = \s ->
    let
      stem = init s ;
      final = last s ;
      fixed = case final of {
        "à" => "â" ;
        "è" => "ê" ;
        "ì" => "î" ;
        "ò" => "ô" ;
        _ => final
      } ;
    in
      stem + fixed ;

  nasAssim : Str -> Str = \s ->
    let
      f = init s ;
    in
      case f of {
        "p" | "m" => "m" ;
        #cons => "n" ;
        _ => "m"
      } ;


-- the proper phonology

  epenthesis : Str -> Str =
  \s ->
  let
    context = Predef.take 4 s ;
    rest = Predef.drop 4 s ;
  in
    case context of {
      a@(#dicons) + b@(#vowel | #glvowel) + c => s ;
      a@(#vowel) + b@(#cons) + "c" + c@(#cons) + d => a + b + "qui" + c + d + rest ;
      a@(#cons) + "co" + b => a + "oco" + b + rest ;
      "c" + b@(#cons) + c => "qui" + b + c + rest ;
      a@(#cons) + b@(#cons) + c => a + "i" + b + c + rest ;
      _ => s 
    } ;

  object3sg : Str -> Str = \s ->
    case s of {
      "i" | "ī" | "e" | "ē" | "ì" | "è" => "qu" ;
      _ => "c"
    } ; 

  lengthen : Str -> Str =
  \s ->
    case s of {
      "a" => "ā" ;
      "e" => "ē" ;
      "i" => "ī" ;
      "o" => "ō" ;
      _ => s
    } ;

  plLengthen : Str -> Str =
  \s ->
    case s of {
      "à" => "ā" ;
      "è" => "ē" ;
      "ì" => "ī" ;
      "ò" => "ō" ;
      _ => lengthen s
    } ;

}