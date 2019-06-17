      subroutine ehg106(il,ir,k,nk,p,pi,n)
      integer execnt,i,ii,il,ir,j,k,l,n,nk,r
      integer pi(n)
      real t
      real p(nk,n)
      save execnt
      data execnt /0/
      execnt=execnt+1
c     find the $k$-th smallest of $n$ elements
c     Floyd+Rivest, CACM Mar '75, Algorithm 489
      l=il
      r=ir
c     top of while loop
    3 if(.not.(l.lt.r))goto 4
c        to avoid recursion, sophisticated partition deleted
c        partition $x sub {l..r}$ about $t$
         t=p(1,pi(k))
         i=l
         j=r
         ii=pi(l)
         pi(l)=pi(k)
         pi(k)=ii
         if(t.lt.p(1,pi(r)))then
            ii=pi(l)
            pi(l)=pi(r)
            pi(r)=ii
         end if
c        top of while loop
    5    if(.not.(i.lt.j))goto 6
            ii=pi(i)
            pi(i)=pi(j)
            pi(j)=ii
            i=i+1
            j=j-1
c           top of while loop
    7       if(.not.(p(1,pi(i)).lt.t))goto 8
               i=i+1
               goto 7
c           bottom of while loop
    8       continue
c           top of while loop
    9       if(.not.(t.lt.p(1,pi(j))))goto 10
               j=j-1
               goto 9
c           bottom of while loop
   10       goto 5
c        bottom of while loop
    6    if(p(1,pi(l)).eq.t)then
            ii=pi(l)
            pi(l)=pi(j)
            pi(j)=ii
         else
            j=j+1
            ii=pi(r)
            pi(r)=pi(j)
            pi(j)=ii
         end if
         if(j.le.k)then
            l=j+1
         end if
         if(k.le.j)then
            r=j-1
         end if
         goto 3
c     bottom of while loop
    4 return
      end
