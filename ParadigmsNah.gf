resource ParadigmsNah = open ResNah, Prelude in {

-- Noun paradigms.

oper
	  
	mkN = overload {
	  mkN : (sg,pl : Str) -> Noun =
	    \sg,pl ->
	      mkN_base sg pl (possStem Animate sg) Animate ;
	    -- if a noun has a sg and pl form, it must be animate

	  mkN : (sg : Str) -> PluralType -> Noun =
	    \sg,pt ->
	      mkN_base sg (nounPlural sg pt) (possStem Animate sg) Animate ;
	    -- many noun plural forms are generic; you can just give their type

	  mkN : (sg : Str) -> Noun =
	    \sg ->
	      case sg of {
	        cal + v@(#glvowel) =>
	          mkN_base sg (cal + (glottalize v) + "quê") (cal + (glottalize v) + "cāuh") Animate ;
	        -- nouns with final glottal vowels are animate "possessives" with
	        -- a distinctive and predictable plural form
	        mexi'ca + "yōtl" => mkN_base sg "" (mexi'ca + "yo") Inanimate ;
	        -- distinctive possessive form for abstract nouns in -yōtl
	        _ => mkN_base sg "" (possStem Inanimate sg) Inanimate
	         -- otherwise, assume that a noun with just one stem is inanimate
	         -- and has no plural stem whatsoever
	      } ;   
	} ;

	mkN_base : (sg,pl,poss : Str) -> Animacy -> Noun =
	  \sg,pl,poss,anim ->
	    {
	      animacy = anim ;				-- only animates can be pluralized!
	      s = noun sg pl poss anim ;
	    } ;


	-- Paradigms for other nominals.

	mkA = overload {

	  mkA : Str -> PluralType -> Adjective =
	  	\sg,pt ->
	  		mkA_base sg (nounPlural sg pt) (possStem Animate sg) ;
	  	-- plenty of predictable plural formations in adjectives ...

	  mkA : Str -> Adjective =
	    \sg ->
	      case sg of {
	        cal + v@(#glvowel) =>
	        	mkA_base sg (cal + (glottalize v) + "quê") (cal + (glottalize v) + "cāuh") ;
	        -- see above inre: possessive nouns; adjectives are the same
	        mic + ("qui" | "c") =>
	        	mkA_base sg (mic + "quê") (mic + "cāuh") ;
	        -- a large set of adjectives end in what looks like the preterite suffix
	        -- and pluralize as such
	        _ => mkA_base sg "" (possStem Inanimate sg) -- this should never happen
	      } ;

	  mkA : Str -> Str -> Adjective =
	    \sg,pl ->
	      mkA_base sg pl (possStem Inanimate sg) ;
	      -- not sure when exactly you'd use this...
	} ;

	mkA_base : (sg,pl,poss : Str) -> Adjective =
	  \sg,pl,poss -> {s = noun sg pl poss Animate } ;

	mkDet = overload {
	  mkDet : Str -> Number -> Det =
		\sg,num -> mkDet_base sg sg "" num ;
	  mkDet : (sg,pl : Str) -> Number -> Det =
	  	\sg,pl,num -> mkDet_base sg pl "" num ;
	  mkDet : (sg,pl,p : Str) -> Number -> Det = mkDet_base
	} ;

	mkDet_base : (sg,pl,p : Str) -> Number -> Det =
	  \sg,pl,p,num ->
	  {
	    pref = table {
	        Sg => sg ;
	        Pl => pl
	      } ;
	    post = p ;
	    number = num ;
	  } ;

	-- Intransitive verb paradigms.

	mkV = overload {
	  mkV : Str -> IVerb = \b1 ->
	    mkV_base
	      b1
	      (base2 Itv b1) 
	      (base3 b1) 
	      (impersonalStem b1) 
	      (causativeStem b1) 
	      (applicativeStem b1)
	      (causativeStem b1) ;
	  -- other sorts to be added, capturing partial predictability...
	} ;

	mkV_base : (b1,b2,b3,impers,c1,a1,h1 : Str) -> IVerb =
	-- intransitives do not have a "base 4",
	-- but some have an unpredictable impersonal
	  \b1,b2,b3,impers,c1,a1,h1 ->
	  let
	    c2 = (base2 Tv c1) ;
	    c3 = (base3 c1) ;
	    c4 = (base4 c1) ;
	    a2 = (base2 Tv a1) ;
	    a3 = (base3 a1) ;
	    a4 = (base4 a1) ;
	    h2 = (base2 Tv c1) ;
	    h3 = (base3 c1) ;
	    h4 = (base4 c1) ;
	  in
	  {
	    active = ivActive b1 b2 b3 ;
	    impersonal = ivImpersonal b1 b2 b3 impers ;
	    causative = tvActive c1 c2 c3 ;
	    causative_passive = tvPassive c1 c2 c3 c4 ;
	    applicative = tvActive a1 a2 a3 ;
	    applicative_passive = tvPassive a1 a2 a3 a4 ;
	    honorific = ivDeponentActive h1 h2 h3 ;
	  } ;


	-- Transitive verb paradigms.

	mkV2 = overload {
	  mkV2 : Str -> TVerb =
	    \b1 ->
	    let
	      b4 = (base4 b1) ;
	      c1 = b4 + "tia" ;
	    in
	      mkV2_base b1
	        (base2 Tv b1)
	        (base3 b1)
	        b4
	        c1
	        (applicativeStem b1)
	        (honorificStem b1) ; -- hon
	} ;

	mkV2_base : (b1,b2,b3,b4,c1,a1,h1 : Str) -> TVerb =
	  \b1,b2,b3,b4,c1,a1,h1 -> 
	  let
	    c2 = (base2 Tv c1) ;
	    c3 = (base3 c1) ;
	    c4 = (base4 c1) ;
	    a2 = (base2 Dtv a1) ;
	    a3 = (base3 a1) ;
	    a4 = (base4 a1) ;
	    h2 = (base2 Dtv a1) ;
	    h3 = (base3 a1) ;
	    h4 = (base4 a1) ;
	  in
	  {
	    active = tvActive b1 b2 b3 ;
	    passive = tvPassive b1 b2 b3 b4 ;
	    causative = tvActiveDerived c1 c2 c3 c4 ;
	    causative_passive = dtvPassive c1 c2 c3 c4 ;
	    applicative = tvActiveDerived a1 a2 a3 a4 ;
	    applicative_passive = dtvPassive a1 a2 a3 a4 ;
	    honorific = tvDeponentActive h1 h2 h3 ;
	  } ;


	-- Intransitive deponent paradigms.

	mkVDep = overload {
	  mkVDep : Str -> DeponentIVerb =
	    \b1 ->
	    let
	      b2 = (base2 Tv b1)
	    in
	      mkVDep_base
	        b1 
	        b2
	        (base3 b1) 
	        (base4 b1) 
	        (applicativeStem b1)
	        (b2 + "tzinoa") ;
	} ;

	mkVDep_base : (b1,b2,b3,b4,a1,h1 : Str) -> DeponentIVerb =
	  \b1,b2,b3,b4,a1,h1 ->
	  let
	    a2 = (base2 Tv a1) ;
	    a3 = (base3 a1) ;
	    a4 = (base4 a1) ;
	    h2 = (base2 Tv h1) ;
	    h3 = (base3 h1) ;
	    h4 = (base4 h1) ;
	  in
	  {
	    active = ivDeponentActive b1 b2 b3 ;
	    impersonal = ivDeponentImpersonal b1 b2 b3 b4 ;
	    causative = tvActive b1 b2 b3 ;
	    causative_passive = tvPassive b1 b2 b3 b4 ;
	    applicative = tvDeponentDerived a1 a2 a3 ;
	    applicative_passive = tvDeponentPassive a1 a2 a3 a4 ;
	    honorific = ivDeponentActive h1 h2 h3 ;
	  } ;


	-- Ditransitive verb paradigms.

	mkV3 = overload {
	  mkV3 : Str -> DTVerb =
	    \b1 ->
	      mkV3_base
	        b1 
	        (base2 Dtv b1) 
	        (base3 b1) 
	        (base4 b1)
	        (applicativeStem b1) ; -- honorific
	} ;

	mkV3_base : (b1,b2,b3,b4,h1 : Str) -> DTVerb =
	  \b1,b2,b3,b4,h1 ->
	  let
	    h2 = (base2 Dtv h1) ;
	    h3 = (base3 h1) ;
	    h4 = (base4 h1) ;
	  in
	  {
	    active = dtvActive b1 b2 b3 b4 ;
	    passive = dtvPassive b1 b2 b3 b4 ;
	    honorific = dtvHonorific h1 h2 h3 h4 ;
	  } ;


	-- Transitive deponent paradigms.

	mkV2Dep = overload {
	  mkV2Dep : Str -> DeponentTVerb =
	    \b1 ->
	    let
	      b2 = (base2 Tv b1) ;
	    in
	      mkV2Dep_base 
	      b1 
	      b2
	      (base3 b1) 
	      (base4 b1)
	      (b2 + "tzinoa") ;
	} ;

	mkV2Dep_base : (b1,b2,b3,b4,h1 : Str) -> DeponentTVerb =
	  \b1,b2,b3,b4,h1 ->
	  let
	    h2 = (base2 Tv h1) ;
	    h3 = (base3 h1) ;
	  in
	  {
	    active = tvDeponentActive b1 b2 b3 ;
	    passive = tvDeponentPassive b1 b2 b3 b4 ;
	    causative = dtvActive b1 b2 b3 b4 ;
	    causative_passive = dtvPassive b1 b2 b3 b4 ;
	    honorific = tvDeponentActive h1 h2 h3 ;
	  } ;

}