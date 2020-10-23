package homework_3

import utest._

object Test extends TestSuite{

    val tests = Tests{
		case class vec3(x: Double, y: Double, z: Double)
		val v = vec3(0.3, 0.4, 0.5)
		val vStr = "vec3(0.3,0.4,0.5)"
		val trueStr = "правда"
		val char = 'A'
		val charStr = "A"

        'test_prettyBooleanFormatter1 - {
            assert(Exercises.prettyBooleanFormatter1(true) == trueStr)
            assert(Exercises.prettyBooleanFormatter1(v) == vStr)
			assert(Exercises.prettyBooleanFormatter1(char) == charStr)
        }

		'test_prettyBooleanFormatter2 - {
            assert(Exercises.prettyBooleanFormatter2(true) == trueStr)
            assert(Exercises.prettyBooleanFormatter2(v) == vStr)
			assert(Exercises.prettyBooleanFormatter2(char) == charStr)
		}

		'test_prettyBooleanFormatter3 - {
            assert(Exercises.prettyBooleanFormatter3(true) == trueStr)
            assert(Exercises.prettyBooleanFormatter3(v) == vStr)
			assert(Exercises.prettyBooleanFormatter3(char) == charStr)
		}

		'test_max1 - {
			val e = intercept[Exception] {
				Exercises.max1(Seq())
			}
			assert(Exercises.max1(Seq(1, -3, 127)) == 127)
			assert(Exercises.max1(Seq(-1)) == -1)
		}

		'test_max2 - {
			assert(Exercises.max2(Seq()) == Seq())
			assert(Exercises.max2(Seq(1, -3, 127)) == Seq(127))
			assert(Exercises.max2(Seq(-1)) == Seq(-1))
		}

		'test_max3 - {
			assert(Exercises.max3(Seq()) == None)
			assert(Exercises.max3(Seq(1, -3, 127)) == Some(127))
			assert(Exercises.max3(Seq(-1)) == Some(-1))
		}

		'test_sum1 - {
			assert(Exercises.sum1(1, 2) == 3)
			assert(Exercises.sum1(0, 2) == 2)
			assert(Exercises.sum1(-1, 1) == 0)
		}

		'test_sum2 - {
			assert(Exercises.sum2(1, 2) == 3)
			assert(Exercises.sum2(0, 2) == 2)
			assert(Exercises.sum2(-1, 1) == 0)
		}

		'test_sum3 - {
			assert(Exercises.sum3(1, 2) == 3)
			assert(Exercises.sum3(0, 2) == 2)
			assert(Exercises.sum3(-1, 1) == 0)
		}
    }
}
