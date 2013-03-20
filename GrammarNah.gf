concrete GrammarNah of Grammar = open ResNah, ParadigmsNah, Prelude in {

flags coding=utf8;

  lincat
    S = {s : Str} ;
    Cl = {s : TAM => Str; nom : Bool} ;
    NP = ResNah.NP ;
    VP = ResNah.VP ;
    AP = {s : AgrS => Str} ;
    CN = ResNah.Noun ;
    Det = ResNah.Det ;
    N = ResNah.Noun ;
    A = ResNah.Adjective ;
    V = IVerb ;
    V2 = TVerb ;
    AdA = {s : Str} ;
    Tense = {t : TAM} ;
    Conj = {s : Str} ;

  lin

    UseCl t cl = 
    let
      particle = case <cl.nom,t.t> of {<True,Present> => "ca"; _ => ""} ;
    in
    {s = particle ++ cl.s ! t.t} ;

    PredVP np vp =
    let
      agr = (SpecificS np.person np.number) ;
     in
      {
        s = case vp.nom of {
            True => \\t => (vp.v ! agr ! t) ++ (vp.c ! agr) ++ np.s ;
            _ => \\t => (vp.v ! agr ! t) ++ np.s ++ (vp.c ! agr)
          } ;
        nom = vp.nom ;
      } ;

    ComplV2 v2 np =
    let
      agr = (SpecificDO np.person np.number) ;
    in
      {
        v = \\s,t => (v2.active ! s ! agr ! NoDirection ! t) ;
        c = \\_ => np.s ;
        nom = False ;
      } ;

    DetCN det cn =
    let
      num = (handleNumber det.number cn.animacy)
    in
    {
      animacy = cn.animacy ;
      number = num ;
      person = P3 ;
      specificity = Specific ;
      s = (det.pref ! num) ++ (cn.s ! (SpecificS P3 num) ! Absolute ! Absolutive) ++ det.post ;
    } ;

    AdjCN ap cn =
    {
      animacy = cn.animacy ;
      s = \\agr,pos,k => (ap.s ! agr) ++ (cn.s ! agr ! pos ! k) ;
    } ;

    CompAP ap =
    {
      v = ca_V ;
      c = ap.s ;
      nom = True ;
    } ;

    AdAP ada ap =
    {
      s = \\agr => ada.s ++ (ap.s ! agr) ;
    } ;

    UseV v =
    {
      v = \\s,t => (v.active ! s ! NoDirection ! t) ;
      c = \\_ => "" ;
      nom = False ;
    } ;

    UseN n = n ;

    UseA a = {s = \\agr => a.s ! agr ! Absolute ! Absolutive} ;

    Pres = {t = Present} ;
    Perf = {t = Preterite Augmented} ;

    a_Det = mkDet "" Sg ;
    every_Det = mkDet "moch" "mochintin" Pl ;
    the_Det = mkDet "in" Sg ;
    this_Det = mkDet "in" "in" "in" Sg ;
    these_Det = mkDet "in" "in" "in" Pl ;
    that_Det = mkDet "in" "in" "on" Sg ;
    those_Det = mkDet "in" "in" "on" Pl ;

    i_NP = pronNP "nèhuātl" P1 Sg ;
    she_NP = pronNP "yèhuātl" P3 Sg ;
    we_NP = pronNP "nèhuāntin" P1 Pl ;

    very_AdA = {s = "cencâ"} ;
    and_Conj = {s = "īhuān"} ;
    or_Conj = {s = "nozo"} ;

    ConjNP co nx ny = {
      animacy = conjAnimacy nx.animacy ny.animacy ;
      number = conjNumber nx.number ny.number ;
      person = conjPerson nx.person ny.person ;
      specificity = conjSpecificity nx.specificity ny.specificity ;
      s = nx.s ++ co.s ++ ny.s
    } ;

    ConjS co x y = {s = x.s ++ co.s ++ y.s} ;

}