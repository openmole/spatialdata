package org.openmole.spatialdata.application.matsim

import org.openmole.spatialdata.utils.math.{Stochastic, Time}
import org.openmole.spatialdata.vector.Point

import scala.util.Random


case class SpenserSynthPop(
                          individuals: Seq[SpenserSynthPop.Individual],
                          households: Seq[SpenserSynthPop.Household],
                          LADs: Seq[String] = Seq.empty
                          ) {

  /**
    * Sample a given proportion of individuals (keep all associated households)
    * @param proportion proportion
    * @return
    */
  def sample(proportion: Double)(implicit rng: Random): SpenserSynthPop = {
    val keptIndivs = Stochastic.sampleWithoutReplacement(individuals, (proportion*individuals.size).toInt)
    val hids = keptIndivs.map(_.householdId)
    val keptHouseholds = households.filter(h => hids.contains(h.hid))
    this.copy(individuals=keptIndivs, households = keptHouseholds)
  }
}


object SpenserSynthPop {

  /**
    * Spenser individual, enriched with home locations, job locations and plans
    *
    * @param id id
    * @param msoaCode home oa
    * @param sex sex
    * @param age age
    * @param householdId household id
    * @param homeLocation home
    * @param workLocation work
    * @param plans plans
    */
  case class Individual(
                       id: String,
                       msoaCode: String,
                       sex: Sex,
                       age: Int,
                       householdId: String, // do not store the full household to avoid redundancy
                       homeLocation: Point = (0.0,0.0),
                       workLocation: Point = (0.0,0.0),
                       plans: Seq[Plan] = Seq.empty
                       )

  object Individual {

    /**
      * construct from csv fields
      * @param fields fields
      * @return
      */
    def apply(fields: Map[String,String]): Individual = Individual(
      id = fields.getOrElse("PID","-1"),
      msoaCode = fields.getOrElse("Area","NA"),
      sex = fields.getOrElse("DC1117EW_C_SEX","0") match {case "0" => Sex; case "1" => Male; case "2" => Female; case _ => Sex},
      age = fields.getOrElse("DC1117EW_C_AGE","0").toInt,
      // ignore ethnic variable: too much modalities
      householdId = fields.getOrElse("HID","-1")
    )

    val csvIndices = Seq(0,1,2,3,5)

    /**
      * column indices
      * @param row row
      * @param indices indices
      * @return
      */
    def apply(row: Array[String], indices: Seq[Int], area: String): Individual = Individual(
      id = area+"_"+row(indices.head),
      msoaCode = row(indices(1)),
      sex = row(indices(2)) match {case "0" => Sex; case "1" => Male; case "2" => Female; case _ => Sex},
      age = row(indices(3)).toInt,
      // ignore ethnic variable: too much modalities
      householdId = area+"_"+row(indices(4))
    )


  }

  sealed trait Sex
  case object Sex extends Sex
  case object Male extends Sex
  case object Female extends Sex

  /**
    * Spenser household
    *
    * @param hid household id
    * @param oaCode home output area
    * @param housingType housing type
    * @param tenureType tenure type
    * @param persons number of persons
    * @param rooms rooms
    * @param bedrooms bedrooms
    * @param centralHeating central heating
    * @param refPersonJobCategory job category of referent person
    * @param cars number of cars
    * @param refPersonId id of referent person
    * @param filled filled by the spenser algorithm
    * @param homeLocation home location (enriched)
    */
  case class Household(
                      hid: String,
                      oaCode: String,
                      housingType: HousingType.HousingType,
                      tenureType: TenureType.TenureType,
                      persons: Int,
                      rooms: Int,
                      bedrooms: Int,
                      centralHeating: Boolean,
                      refPersonJobCategory: JobCategory.JobCategory,
                      cars: Int,
                      refPersonId: Int,
                      filled: Boolean,
                      homeLocation: Point = (0.0,0.0)
                      )

  object Household {

    /**
      * construct from csv fields
      * @param fields fields
      * @return
      */
    def apply(fields: Map[String,String]): Household = {
      Household(
        hid = fields.getOrElse("HID","-1"),
        oaCode = fields.getOrElse("Area","NA"),
        housingType = HousingType(fields.getOrElse("LC4402_C_TYPACCOM","-1")),
        // QS420_CELL land-use: not filled?
        tenureType = TenureType(fields.getOrElse("LC4402_C_TENHUK11","-1")),
        // LC4408_C_AHTHUK11 ignored
        // CommunalSize ignored
        persons = fields.getOrElse("LC4404_C_SIZHUK11","0").toInt,
        rooms = fields.getOrElse("LC4404_C_ROOMS","0").toInt,
        bedrooms = fields.getOrElse("LC4405EW_C_BEDROOMS","0").toInt,
        // LC4408EW_C_PPBROOMHEW11 ignored
        centralHeating =  fields.getOrElse("LC4402_C_CENHEATHUK11","0") match {case "2" => true; case _ => false},
        refPersonJobCategory = JobCategory(fields.getOrElse("LC4605_C_NSSEC","0")),
        //LC4202_C_ETHHUK11 ignored
        cars= fields.getOrElse("LC4202_C_CARSNO","0").toInt - 1,
        refPersonId = fields.getOrElse("HRPID","0").toInt,
        filled = fields.getOrElse("FILLED","False") match {case "True" => true; case _ => false}
      )
    }

    val csvIndices = Seq(0,1,2,4,7,8,9,11,12,14,15,16)

    def apply(row: Array[String], indices: Seq[Int], area: String): Household = {
      Household(
        hid = area+"_"+row(indices.head),
        oaCode = row(indices(1)),
        housingType = HousingType(row(indices(2))),
        // QS420_CELL land-use: not filled?
        tenureType = TenureType(row(indices(3))),
        // LC4408_C_AHTHUK11 ignored
        // CommunalSize ignored
        persons = row(indices(4)).toInt,
        rooms = row(indices(5)).toInt,
        bedrooms = row(indices(6)).toInt,
        // LC4408EW_C_PPBROOMHEW11 ignored
        centralHeating =  row(indices(7)) match {case "2" => true; case _ => false},
        refPersonJobCategory = JobCategory(row(indices(8))),
        //LC4202_C_ETHHUK11 ignored
        cars= row(indices(9)).toInt - 1,
        refPersonId =row(indices(10)).toInt,
        filled = row(indices(11)) match {case "True" => true; case _ => false}
      )
    }
  }

  object HousingType extends Enumeration {
    type HousingType = Value
    val All, HouseTotal, HouseDetached, HouseSemiDetached, HouseTerraced, Flat = Value
    def apply(code: String): HousingType = {
      code match {
        case "0" => All
        case "1" => HouseTotal
        case "2" => HouseDetached
        case "3" => HouseSemiDetached
        case "4" => HouseTerraced
        case "5" => Flat
        case _ => All
      }

    }
  }

  object TenureType extends Enumeration {
    type TenureType = Value
    val All, Owned, OwnedOutright, OwnedMortgage, Rented, RentedSocial, RentedPrivate = Value
    def apply(code: String): TenureType = {
      code match {
        case "0" => All
        case "1" => Owned
        case "2" => OwnedOutright
        case "3" => OwnedMortgage
        case "4" => Rented
        case "5" => RentedSocial
        case "6" => RentedPrivate
        case _ => All
      }
    }
  }

  object JobCategory extends Enumeration {
    type JobCategory = Value
    val All, HigherManag, LowerManag, Intermediate, SmallEmployers, Technical, SemiRoutine, Routine, Unemployed, Student = Value
    def apply(code: String): JobCategory = {
      code match {
        case "0" => All; case "1" => HigherManag; case "2" => LowerManag; case "3" => Intermediate
        case "4" => SmallEmployers; case "5" => Technical; case "6" => SemiRoutine
        case "7" => Routine; case "8" => Unemployed; case "9" => Student
        case _ => All
      }
    }
  }


  /**
    * Plan for individual during a day
    *
    * @param actions actions - must have one more than legs (and end in the same place as start?)
    * @param legs legs
    */
  case class Plan(
                  actions: Seq[Plan.Action],
                  legs: Seq[Plan.Leg]
                 )

  object Plan {

    /**
      *
      * Basic commute plan home-work-home
      *
      * @param start work start time
      * @param end work end time
      * @param homeLoc home location
      * @param workLoc work location
      * @param mode mode
      * @return
      */
    def commutePlan(start: Double, end: Double, homeLoc: Point, workLoc: Point, mode: String): Plan = Plan(
      actions = Seq(
        Action("home", homeLoc, "00:00", Time.decimalHourToString(start)),
        Action("work", workLoc, Time.decimalHourToString(start), Time.decimalHourToString(end)),
        Action("home", homeLoc, Time.decimalHourToString(end), "23:59")
      ),
      legs = Seq(Leg(mode), Leg(mode))
    )

    /**
      * ! check that ation coordinates are same format/CRS than network coordinates
      * @param actionType type of action (home, work, ...)
      * @param place place
      * @param startTime start time - format 08:00:00 - 08:00 also fine
      * @param endTime end time
      */
    case class Action(actionType: String, place: Point, startTime: String, endTime: String){
      def xml: String = "<activity type=\""+actionType+"\" x=\""+place._1+"\" y=\""+place._2+"\" start_time=\""+startTime+"\" end_time=\""+endTime+"\" />"
    }
    case class Leg(mode: String) {
      def xml: String = "<leg mode=\""+mode+"\" />"
    }
  }


}
