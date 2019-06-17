      subroutine lowesc(n,l,ll,trl,delta1,delta2)
      integer execnt,i,j,n
      real delta1,delta2,trl
      real l(n,n),ll(n,n)
      real sdot
      external sdot
      save execnt
      data execnt /0/
      execnt=execnt+1
c     compute $LL~=~(I-L)(I-L)'$
      do 3 i=1,n
         l(i,i)=l(i,i)-1
    3 continue
      do 4 i=1,n
         do 5 j=1,i
            ll(i,j)=sdot(n,l(i,1),n,l(j,1),n)
    5    continue
    4 continue
      do 6 i=1,n
         do 7 j=i+1,n
            ll(i,j)=ll(j,i)
    7    continue
    6 continue
      do 8 i=1,n
         l(i,i)=l(i,i)+1
    8 continue
c     accumulate first two traces
      trl=0
      delta1=0
      do 9 i=1,n
         trl=trl+l(i,i)
         delta1=delta1+ll(i,i)
    9 continue
c     $delta sub 2 = "tr" LL sup 2$
      delta2=0
      do 10 i=1,n
         delta2=delta2+sdot(n,ll(i,1),n,ll(1,i),1)
   10 continue
      return
      end
