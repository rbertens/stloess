      subroutine ehg129(l,u,d,x,pi,n,sigma)
      integer d,execnt,i,k,l,n,u
      integer pi(n)
      real machin,alpha,beta,t
      real sigma(d),x(n,d)
      real r1mach
      external r1mach
      save machin,execnt
      data execnt /0/
c     MachInf -> machin
      execnt=execnt+1
      if(execnt.eq.1)then
         machin=r1mach(2)
      end if
      do 3 k=1,d
         alpha=machin
         beta=-machin
         do 4 i=l,u
            t=x(pi(i),k)
            alpha=min(alpha,x(pi(i),k))
            beta=max(beta,t)
    4    continue
         sigma(k)=beta-alpha
    3 continue
      return
      end
