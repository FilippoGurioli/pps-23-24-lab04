package tasks.adts
import u03.Sequences.*
import u03.Optionals.*

/*  Exercise 2: 
 *  Implement the below trait, and write a meaningful test.
 *  Suggestion: 
 *  - reuse Sequences and Optionals as imported above
 *  - Course is a simple case classes with just the name
 *  - Teacher is a case class with name and sequence of courses
 *  - School is a case class with (sequences of) teachers and courses
 *  - add/set methods below create the new school 
 */

object SchoolModel:

  trait SchoolModule:
    type School
    type Teacher
    type Course
    extension (school: School)
      def addTeacher(name: String): School
      def addCourse(name: String): School
      def teacherByName(name: String): Optional[Teacher]
      def courseByName(name: String): Optional[Course]
      def nameOfTeacher(teacher: Teacher): String
      def nameOfCourse(course: Course): String
      def setTeacherToCourse(teacher: Teacher, course: Course): School
      def coursesOfATeacher(teacher: Teacher): Sequence[Course]

  object SchoolModuleImpl extends SchoolModule:
    case class CourseImpl(name: String)
    opaque type Course = CourseImpl

    case class TeacherImpl(name: String, courses: Sequence[Course])
    opaque type Teacher = TeacherImpl

    case class SchoolImpl(teachers: Sequence[Teacher], courses: Sequence[Course])
    opaque type School = SchoolImpl

    extension (school: School) 
      def setTeacherToCourse(teacher: Teacher, course: Course): School = 
        teacherByName(teacher.name) match
          case Optional.Just(t) => 
            courseByName(course.name) match
              //should it check also if the course is already present in teacher's courses?
              //I do not check only because if they want me to do this check they should have used Set instead of Sequence
              case Optional.Just(c) => SchoolImpl( 
                Sequence.Cons(
                  TeacherImpl(teacher.name, Sequence.Cons(c, teacher.courses)), 
                  Sequence.filter(school.teachers)(_.name != t.name)), 
                school.courses
              )
              case _ => school
          case _ => school
        
      def nameOfCourse(course: Course): String = 
        courseByName(course.name) match { case Optional.Just(c) => c.name; case Optional.Empty() => "" }
      def addCourse(name: String): School = SchoolImpl(school.teachers, Sequence.Cons(CourseImpl(name), school.courses))
      
      def courseByName(name: String): Optional[Course] = 
        Sequence.filter(school.courses)(c => c.name == name) match { case Sequence.Cons(h, t) => Optional.Just(h); case Sequence.Nil() => Optional.Empty() }
      def addTeacher(name: String): School = SchoolImpl(Sequence.Cons(TeacherImpl(name, Sequence.Nil()), school.teachers), school.courses)
      def coursesOfATeacher(teacher: Teacher): Sequence[Course] = 
        teacherByName(teacher.name) match
          case Optional.Just(t) => t.courses
          case Optional.Empty() => Sequence.Nil()
        
      def teacherByName(name: String): Optional[Teacher] = 
        Sequence.filter(school.teachers)(t => t.name == name) match { case Sequence.Cons(h, t) => Optional.Just(h); case Sequence.Nil() => Optional.Empty() }
      def nameOfTeacher(teacher: Teacher): String = 
        teacherByName(teacher.name) match { case Optional.Just(t) => t.name; case Optional.Empty() => "" }