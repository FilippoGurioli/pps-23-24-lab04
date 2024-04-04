package tasks

object Task1: //done alone
    object Ex1ComplexNumbers:
        trait ComplexADT:
            type Complex
            def complex(re: Double, im: Double): Complex
            extension (complex: Complex)
                def re(): Double
                def im(): Double
                def sum(other: Complex): Complex
                def subtract(other: Complex): Complex
                def asString(): String

        object BasicComplexADT extends ComplexADT:
            private case class ComplexNumber(re: Double, im: Double)
            opaque type Complex = ComplexNumber
            def complex(re: Double, im: Double): Complex = ComplexNumber(re, im)
            extension (complex: Complex)
                def re(): Double = complex.re
                def im(): Double = complex.im
                def sum(other: Complex): Complex = ComplexNumber(complex.re + other.re, complex.im + complex.im)
                def subtract(other: Complex): Complex = ComplexNumber(complex.re - other.re, complex.im - complex.im)
                def asString(): String = complex match
                    case ComplexNumber(re, im) if re != 0 && im != 0 => re + (if im > 0 then " + " else " - ") + im + "i"
                    case ComplexNumber(re, im) if im == 0 => re + ""
                    case ComplexNumber(re, im) if re == 0 => im + "i"

object Task2: //done alone
    import u03.Sequences.*
    import u03.Optionals.*
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

object Task3:  //done alone
    import u03.Sequences.*
    import u03.Optionals.*
    object Ex3Stacks:
        trait StackADT:
            type Stack[A]
            def empty[A]: Stack[A]
            extension [A](stack: Stack[A])
                def push(a: A): Stack[A]
                def pop(a: A): Optional[(A, Stack[A])]
                def asSequence(): Sequence[A]
        
        object StackImpl extends StackADT:
            type Stack[A] = Sequence[A]
            def empty[A]: Stack[A] = Sequence.Nil()
            extension [A](stack: Stack[A])
                def push(a: A): Stack[A] = Sequence.Cons(a, stack)
                def pop(a: A): Optional[(A, Stack[A])] = stack match
                    case Sequence.Cons(h, t) if h == a => Optional.Just((h, t)) //should it check if the param is equal to the popped top?
                    case _ => Optional.Empty()
                
                def asSequence(): Sequence[A] = stack

object Task4: //done alone
    import u03.Sequences.* 
    import Sequence.*
    object Ex4Summables:
        def sumAllInt(seq: Sequence[Int]): Int = seq match
            case Cons(h, t) => h + sumAllInt(t)
            case _ => 0
        trait Summable[A]:
            def sum(a1: A, a2: A): A
            def zero: A
        def sumAll[A: Summable](seq: Sequence[A]): A = 
            val summable = summon[Summable[A]]
            seq match
            case Cons(head, tail) => 
                summable.sum(head, sumAll(tail))
            case Nil() => summable.zero
            
        given Summable[Int] with
            def sum(a1: Int, a2: Int): Int = a1 + a2
            def zero: Int = 0
        given Summable[Double] with
            def sum(a1: Double, a2: Double): Double = a1 + a2
            def zero: Double = 0
        given Summable[String] with
            def sum(a1: String, a2: String): String = a1 + a2
            def zero: String = ""

object Task5: //done alone
    import u03.Sequences.* 
    import Sequence.*
    import u04.monads.Optionals.Optional
    object Ex5Traversable:
        def log[A](a: A): Unit = println("The next element is: "+a)
        def logAll[A](seq: Sequence[A]): Unit = seq match
            case Cons(h, t) => log(h); logAll(t)
            case _ => ()

        trait Traversable[T[_]]:
            extension [A](t: T[A]) def applyAll(f: A => Unit): Unit
        
        given Traversable[Sequence] with
            extension [A](t: Sequence[A]) def applyAll(f: A => Unit) = t match
                case Cons(h, t) => f(h); t.applyAll(f)
                case _ =>

        given Traversable[Optional] with
            extension [A](t: Optional[A]) def applyAll(f: A => Unit) = t match
            case Optional.Just(a) => f(a)
            case _ => 

object Task6: //done alone
    import u04.monads.Monads.Monad
    import u04.monads.Monads.Monad
    object Ex6TryModel:
        private enum TryImpl[A]:
            case Success(value: A)
            case Failure(exception: Throwable)

        opaque type Try[A] = TryImpl[A]

        def success[A](value: A): Try[A] = TryImpl.Success(value)
        def failure[A](exception: Throwable): Try[A] = TryImpl.Failure(exception)
        def exec[A](expression: => A): Try[A] = 
            try success(expression)
            catch case e: Throwable => failure(e)

        extension [A](m: Try[A]) 
            def getOrElse[B >: A](other: B): B = m match
            case TryImpl.Success(value) => value
            case TryImpl.Failure(_) => other

        given Monad[Try] with
            override def unit[A](value: A): Try[A] = exec(value)
            extension [A](m: Try[A]) 
                override def flatMap[B](f: A => Try[B]): Try[B] = m match
                    case TryImpl.Success(value) => f(value)
                    case TryImpl.Failure(ex) => failure(ex)