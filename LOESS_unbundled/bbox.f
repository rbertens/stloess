      subroutine ehg126(d,n,vc,x,v,nvmax)
      integer d,execnt,i,j,k,n,nv,nvmax,vc
      real machin,alpha,beta,mu,t
      real v(nvmax,d),x(n,d)
      real r1mach
      external r1mach
      save machin,execnt
      data execnt /0/
c     MachInf -> machin
      execnt=execnt+1
      if(execnt.eq.1)then
         machin=r1mach(2)
      end if
c     fill in vertices for bounding box of $x$
c     lower left, upper right
      do 3 k=1,d
         alpha=machin
         beta=-machin
         do 4 i=1,n
            t=x(i,k)
            alpha=min(alpha,t)
            beta=max(beta,t)
    4    continue
c        expand the box a little
         mu=0.005*max(beta-alpha,1.e-10*max(abs(alpha),abs(beta))+1.e-30
     $)
         alpha=alpha-mu
         beta=beta+mu
         v(1,k)=alpha
         v(vc,k)=beta
    3 continue
c     remaining vertices
      do 5 i=2,vc-1
         j=i-1
         do 6 k=1,d
            v(i,k)=v(1+mod(j,2)*(vc-1),k)
            j=float(j)/2.
    6    continue
    5 continue
      return
      end
