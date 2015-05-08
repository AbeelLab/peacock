package tbarc.drugs

object Drug {

  def sorter(x: Drug, y: Drug): Boolean = {
    if (x.line == y.line) {
      val cmp = x.drugClass == y.drugClass
      if (cmp) {
        x.name(0) < y.name(0)
      } else
        x.drugClass < y.drugClass
    } else {
      x.line < y.line
    }
  }

  val drugs = List(
    new Drug(List("cpm", "capreomycin", "capreomycin_c_", "solid_agar__capreomycin__c__10ugml"), 2, "polypeptides", "Capreomycin", "cap"),
    new Drug(List("emb", "ethambutol", "ethambutol_eth", "solid_agar__ethambutol__eth__75ugml"), 1, "first", "Ethambutol", "emb"),
    new Drug(List("eth", "ethionamide", "ethionamide_et", "solid_agar__ethionamide__et__10ugml"), 2, "thioamides", "Ethionamide", "eth"),
    new Drug(List("inh", "isoniazid", "isoniazid_inh", "solid_agar__isoniazid__inh__low_or_high_concentration"), 1, "first", "Isoniazid", "inh"),
    new Drug(List("kan", "kanamycin"), 2, "aminoglycosides", "Kanamycin", "kan"),
    new Drug(List("amk", "amikacin"), 2, "aminoglycosides", "Amikacin", "amk"),
    new Drug(List("cs", "cycloserine"), 2, "cycloserine", "Cycloserine", "cs"),
    new Drug(List("mfx", "moxifloxacin"), 2, "moxifloxacin", "Moxifloxacin", "mfx"),
    new Drug(List("rfb", "rifabutin"), 2, "rifabutin", "Rifabutin", "rfb"),
    new Drug(List("prop","pas", "prop_4aminosalicylic_acid"), 2, "prop_4aminosalicylic_acid", "prop 4aminosalicylic acid", "pas"),
    new Drug(List("ofx", "ofloxacin", "ofloxacin_ofl", "solid_agar__ofloxacin__ofl__20ugml"), 2, "fluoroquinolone (2nd gen)", "Ofloxacin", "ofl"),
    new Drug(List("rmp", "rifampicin", "rifampicin_rif", "solid_agar__rifampicin__rif__10ugml"), 1, "first", "Rifampicin", "rif"),
    new Drug(List("pyrazinamide", "nicotinamide", "niacin_nia", "solid_agar__nicotinamide__nic__500ugml"), 1, "first", "Pyrazinamide", "pza"),
    new Drug(List("str", "streptomycin", "streptomycin_str", "solid_agar_streptomycin__str__20ugml"), 1, "aminoglycosides", "Streptomycin", "str")
    )

  def get(keyFragment: String): List[Drug] = {
    println("finding: " + keyFragment)

    val threeCoded = drugs.filter(f => f.chris3Letter.contains(keyFragment)).toList
    if (threeCoded.size > 0)
      threeCoded
    else
      drugs.filter(f => (f.name.filter(p => p.contains(keyFragment))).size > 0)

  }

  def threeCode(code: String): Drug = {
    val list = drugs.filter(f => f.chris3Letter.contains(code))
    assume(list.size == 1, "Failure for 3code: " + code + "\t" + list)
    list(0)
  }

  def single(keyFragment: String): Drug = {
    
    val list = get(keyFragment.split("\\$")(0)).toSet
    assume(list.size == 1, "Failure for keyfragment: " + keyFragment + "\t" + list)
    list.head
  }
}

class Drug(val name: List[String], val line: Int, val drugClass: DrugClass, val displayName: String, val chris3Letter: String) {

  override def toString(): String = displayName

}

 
