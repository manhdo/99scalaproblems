package org.mdo

import scala.annotation.tailrec

/*
	ninety nine problems
*/
object ScalaProblems {
	
	/*
	P01 (*) Find the last element of a list.
	Example:
	scala> last(List(1, 1, 2, 3, 5, 8))
	res0: Int = 8
	*/
	def last[T](list:List[T]) = {
		list.last
	}
	
	def last2[T](list:List[T]) : T = {
		list match {
			case head::Nil => head
			case head::tail => last2(tail)
			case Nil => throw new NoSuchElementException
		}
	}
	
	/*
	P02 (*) Find the last but one element of a list.
	Example:
	scala> penultimate(List(1, 1, 2, 3, 5, 8))
	res0: Int = 5
	*/
	def beforeLast[T](list:List[T]) = {
		if (list.length > 1) {
			list.apply(list.length-2)
		} else {
			list.last
		}
	}
	
	def beforeLast2[T](list:List[T]) : T = {
		list match {
			case prev::head::Nil => prev
			case head::tail => beforeLast2(tail)
			case Nil => throw new NoSuchElementException
		}
	}
	
	/*
	P03 (*) Find the Kth element of a list.
	By convention, the first element in the list is element 0.
	Example:

	scala> nth(2, List(1, 1, 2, 3, 5, 8))
	res0: Int = 2
	*/
	def nth[T](ndx:Int,list:List[T]) : T =  { 
		list match {
			case head::tail if ndx == 0 => head
			case head::tail => nth(ndx-1,tail)
			case Nil => throw new NoSuchElementException
		}
	}
	
	/*
	P04 (*) Find the number of elements of a list.
	Example:
	scala> length(List(1, 1, 2, 3, 5, 8))
	res0: Int = 6
	*/
	def length[T](list:List[T]) {
		list.length
	}
	
	/*
	P05 (*) Reverse a list.
	Example:
	scala> reverse(List(1, 1, 2, 3, 5, 8))
	res0: List[Int] = List(8, 5, 3, 2, 1, 1)
	*/
	def reverse[T](list:List[T]) {
		list.reverse
	}
	
	/*
	P06 (*) Find out whether a list is a palindrome.
	Example:
	scala> isPalindrome(List(1, 2, 3, 2, 1))
	res0: Boolean = true
	*/
	def isPalindrome[T](list:List[T]) : Boolean =  {
		var left = 0
		var right = list.length-1
		while(left <= right) {
			if (list.apply(left) != list.apply(right)) return false
			left += 1
			right -= 1
		}	
		return true
	}
	
	/*
	P07 (**) Flatten a nested list structure.
	Example:
	scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
	res0: List[Any] = List(1, 1, 2, 3, 5, 8)
	*/
	def flatten[T](list:List[T]) : List[T] = {
		//List.flatten(list)
		//list.flatten
		list match {
			case (head:List[T])::Nil => flatten(head)
			case (head:List[T])::tail => flatten(head):::flatten(tail)
			case head::Nil => List(head)
			case head::tail => head::flatten(tail)
			case Nil => Nil
		}
	}
	
	/*
	P08 (**) Eliminate consecutive duplicates of list elements.
	If a list contains repeated elements they should be replaced with a single copy of the element. 
	The order of the elements should not be changed.
	Example:

	scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
	res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
	*/
	def compress[T](list:List[T]):List[T] = {
		list match {
			case head::next::tail if head == next => compress(head::tail)
			case head::tail => head::compress(tail)
			case Nil => List()
		}
	}
	
	/*
		Tail Recursion
		compress2(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
	*/
	def compress2[T](list:List[T]):List[T] = {
		@tailrec
		def compress3(result:List[T], rl:List[T]) : List[T] = {
			rl match {
				case head::next::tail if head == next => compress3(result,head::tail)
				case head::tail => compress3(result:::List(head),tail)
				case Nil => result
			}
		}
		compress3(Nil,list)
	}
	/*
	P09 (**) Pack consecutive duplicates of list elements into sublists.
	If a list contains repeated elements they should be placed in separate sublists.
	Example:

	scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
	res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), 
	List('d), List('e, 'e, 'e, 'e))
	
	Uses: Tail recursion
	*/
	def pack[T](list:List[T]) : List[List[T]] = {
		@tailrec
		def pack2(result:List[List[T]],sl:List[T],rl:List[T]) :List[List[T]] = {
			rl match {
				case head::tail if sl.exists(x=> x == head) =>pack2(result,head::sl,tail)
				case head::tail => 
					pack2((if(sl.length > 0) result:::List(sl) else result),List(head),tail)
				case Nil => result:::List(sl) 
			}
		}
		
		pack2(Nil,List(),list)
	}
	
	/*
	P10 (*) Run-length encoding of a list.
	Use the result of problem P09 to implement the so-called run-length encoding data compression method.
	 Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
	Example:

	scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
	res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
	*/
	def encode[T](list:List[T]) : List[(Int,T)] = {
		@tailrec
		def encode2(result:List[(Int,T)], rl:List[List[T]]) : List[(Int,T)] = {
			rl match {
				case head::tail => encode2(result ::: List((head.length,head.head)), tail)
				case Nil => result
			}
		}
		encode2(Nil,pack(list))
	}
	
	/*
	P11 (*) Modified run-length encoding.
	Modify the result of problem P10 in such a way that if an element has no duplicates 
	it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.
	Example:

	scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
	res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
	*/
	def encodeModified[T](list:List[T]) : List[Any] = {
		@tailrec
		def encodeModified2(result:List[Any], rl:List[(Int,T)]) : List[Any] = {
			rl match {
				case (n,v)::tail if n > 1 => encodeModified2(result:::List((n,v)),tail)
				case (n,v)::tail => encodeModified2(result:::List(v),tail)
				case Nil => result
			}
		}
		encodeModified2(Nil,encode(list))
	}
	
	/*
	P12 (**) Decode a run-length encoded list.
	Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
	Example:

	scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
	res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
	*/
	def decode[T](list:List[(Int,T)]) : List[T] = {
		
		def range[T](result:List[T], n:Int,v:T) : List[T] = {
			n match {
				case n if n > 0 => range(v::result,(n-1),v)
				case _ => result
			}
		}
		
		def decode2(result:List[T], rl:List[(Int,T)]) : List[T] = {
			rl match {
				case (n,v)::tail => decode2(result:::range(Nil,n,v),tail)
				case Nil => result
			}
		}
		decode2(Nil,list)
	}
	
	/*
	P13 (**) Run-length encoding of a list (direct solution).
	Implement the so-called run-length encoding data compression method directly. I.e. 
	don't use other methods you've written (like P09's pack); do all the work directly.
	Example:

	scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
	res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
	*/
	def encodeDirect[T](list:List[T]) : List[(Int,T)] = {
		
		def encode(result:List[(Int,T)], n:Int, rl:List[T]) : List[(Int,T)] = {
			rl match {
				case head::next::tail if head == next => encode(result,n+1,next::tail)
				case head::next::tail => encode(result:::List((n+1,head)),0,next::tail)
				case head::Nil => result:::List((n+1,head))
				case Nil => result
			}
		}
		
		encode(Nil,0,list)
	}
	
	/*
	P14 (*) Duplicate the elements of a list.
	Example:
	scala> duplicate(List('a, 'b, 'c, 'c, 'd))
	res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
	*/
	def duplicate[T](list:List[T]) : List[T] = {
		def duplicate2(result:List[T],rl:List[T]) : List[T] = {
			rl match {
				case head::tail => duplicate2((result:::List(head)):::List(head),tail)
				case Nil => result
			}
		}
		duplicate2(Nil,list)
	}
	
	/*
	P15 (**) Duplicate the elements of a list a given number of times.
	Example:
	scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
	res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
	*/
	def duplicateN[T](count:Int,list:List[T]) : List[T] = {
		
		def range[T](result:List[T], n:Int,v:T) : List[T] = {
			if (n > 0) range(v::result,(n-1),v)
			else result
		}
		
		def duplicate(result:List[T], n:Int, rl:List[T]) : List[T] = {
			rl match {
				case head::tail => duplicate(result:::(range(Nil,n,head)), n,tail)
				case _ => result
			}
		} 
		
		duplicate(Nil,count,list)
	}
	
	/*
	P16 (**) Drop every Nth element from a list.
	Example:
	scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
	res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
	*/
	def drop[T](n:Int, list:List[T]) : List[T] = {
		def drop2(result:List[T], ndx:Int, cndx:Int, rl:List[T]) : List[T] = {
			rl match {
				case head::tail if cndx > 1 => drop2(result:::List(head),ndx,cndx-1,tail)
				case head::tail => drop2(result,ndx,ndx,tail)
				case _ => result
			}
		}
		drop2(Nil,n,n,list)
	}
	
	/*
	P17 (*) Split a list into two parts.
	The length of the first part is given. Use a Tuple for your result.
	Example:

	scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
	res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
	*/
	def split[T](length:Int,list:List[T]) : (List[T],List[T]) = {
		def split2(result:(List[T],List[T]),n:Int,rl:List[T]) : (List[T],List[T]) = {
			
			(result,rl) match {
				case ((left,right),head::tail) if n > 0 => split2(((left:::List(head)),right),n-1,tail)
				case ((left,right),head::tail) => split2((left,(right:::List(head))),n,tail)
				case (_,_) => result
			}
		}
		split2((Nil,Nil),length,list)
	}
	
	/*
	P18 (**) Extract a slice from a list.
	Given two indices, I and K, the slice is the list containing the elements from and including the 
	Ith element up to but not including the Kth element of the original list. 
	Start counting the elements with 0.
	Example:

	scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
	res0: List[Symbol] = List('d, 'e, 'f, 'g)
	*/
	def slice[T](start:Int, end:Int, list:List[T]) : List[T] = {
		def slice2(s:Int,e:Int,result:List[T],rl:List[T]) : List[T] = {
			rl match {
				case head::tail if s > 0 => slice2(s-1,e-1,result,tail)
				case head::tail if s == 0 && e > 0 => slice2(s,e-1,result:::List(head),tail)
				case _ => result
			}
		}
		return slice2(start,end,Nil,list)
	}
	
	/*
	P19 (**) Rotate a list N places to the left.
	Examples:
	scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
	res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)

	scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
	res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
	*/
	def rotate[T](pos:Int,list:List[T]) : List[T] = {
		def rotate2(p:Int,rl:List[T]) : List[T] = {
			rl match {
				case head::tail if p > 0 => rotate2(p-1,tail:::List(head))
				case _ => rl
			}
		}
		if (pos >= 0) rotate2(pos,list)
		else rotate2(list.length + pos,list)
	}
	
	/*
	P20 (*) Remove the Kth element from a list.
	Return the list and the removed element in a Tuple. Elements are numbered from 0.
	Example:

	scala> removeAt(1, List('a, 'b, 'c, 'd))
	res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
	*/
	def removeAt[T](ndx:Int,list:List[T]):(List[T],T) = {
		def removeAt2(n:Int,result:(List[T],T),rl:List[T]) : (List[T],T) = {
			(result,rl) match {
				case ((left,right),head::tail) if n > 0 => removeAt2(n-1,(left:::List(head),right),tail)
				case ((left,right),head::tail) if n == 0 => removeAt2(n-1,(left,head),tail)
				case ((left,right),head::tail) => removeAt2(n,(left:::List(head),right),tail)
				case (_,Nil) => result
			}
			
		}
		removeAt2(ndx,(Nil,list.last),list)
	}
	
	/*
	P21 (*) Insert an element at a given position into a list.
	Example:
	scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
	res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
	*/
	def insertAt[T](item:T,ndx:Int,list:List[T]) : List[T] = {
		def insertAt2(itm:T,n:Int,result:List[T],rl:List[T]) : List[T] = {
			rl match {
				case head::tail if n > 0 => insertAt2(itm,n-1,result:::List(head),tail)
				case head::tail if n == 0 => insertAt2(itm,n-1,(result:::List(itm)):::List(head),tail)
				case head::tail => insertAt2(itm,n,result:::List(head),tail)
				case _ => result
			}
		}
		if (ndx > (list.length-1)) list:::List(item)
		else insertAt2(item,ndx,Nil,list)
	}
	
	/*
	P22 (*) Create a list containing all integers within a given range.
	Example:
	scala> range(4, 9)
	res0: List[Int] = List(4, 5, 6, 7, 8, 9)
	*/
	def range(start:Int,end:Int): List[Int] = {
		def range2(result:List[Int],current:Int,max:Int) : List[Int] = {
			(current,max) match {
				case _ if current <= max => range2(result:::List(current),current+1,max)
				case _ => result
			}
		}
		range2(Nil,start,end)
 	}

	/*
	P23 (**) Extract a given number of randomly selected elements from a list.
	Example:
	scala> randomSelect(3, List('a, 'b, 'c, 'd, 'e,'f, 'g, 'h))
	res0: List[Symbol] = List('e, 'd, 'a)
	Hint: Use the solution to problem P20
	*/
	import scala.util.Random
	def randomSelect[T](num:Int,list:List[T]):List[T] = {
		val rand = new Random
		def randomSelect2(result:List[T],n:Int,rl:List[T]) : List[T] = {
			n match {
				case _ if n > 0 => 
					val (xs,x) = removeAt(rand.nextInt(rl.length),rl)
					randomSelect2(result:::List(x),n-1,xs)
				case _ => result
					
			}
		}
		randomSelect2(Nil,num,list)
	}
	
	/*
	P24 (*) Lotto: Draw N different random numbers from the set 1..M.
	Example:
	scala> lotto(6, 49)
	res0: List[Int] = List(23, 1, 17, 33, 21, 37)
	*/
	def lotto(num:Int,max:Int) : List[Int] = {
		randomSelect(num,range(1,max))
	}
	
	/*
	P25 (*) Generate a random permutation of the elements of a list.
	Hint: Use the solution of problem P23.
	Example:

	scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
	res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)
	*/
	def randomPermute[T](list:List[T]) : List[T] = {
		randomSelect(list.length,list)
	}
	
	/*
	P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list.
	In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that 
	there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficient). 
	For pure mathematicians, this result may be great. But we want to really generate all the possibilities.
	Example:

	scala> combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
	res0: List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ..
	*/
	def combinations[T](num:Int,list:List[T]):List[T] = {
		def combinations2(results:Int,k:Int,current:Int, pos:Int, list:List[T]) : List[T] = {
			Nil
		}
		Nil
	}
	
	def run(args: Array[String]) {
		val list = List(1,2,3,4,5,5,4,3,2,1)
		println(isPalindrome(list))
	}
}

