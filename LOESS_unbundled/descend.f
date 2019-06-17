      integer function ehg138(i,z,a,xi,lo,hi,ncmax)
      logical i1
      integer d,execnt,i,j,nc,ncmax
      integer a(ncmax),hi(ncmax),lo(ncmax)
      real xi(ncmax),z(8)
      save execnt
      data execnt /0/
      execnt=execnt+1
c     descend tree until leaf or ambiguous
      j=i
c     top of while loop
    3 if(a(j).ne.0)then
         i1=(z(a(j)).ne.xi(j))
      else
         i1=.false.
      end if
      if(.not.(i1))goto 4
         if(z(a(j)).le.xi(j))then
            j=lo(j)
         else
            j=hi(j)
         end if
         goto 3
c     bottom of while loop
    4 ehg138=j
      return
      end
