      subroutine ehg127(q,n,d,nf,f,x,psi,y,rw,kernel,k,dist,eta,b,od,w,r
     $cond,sing,sigma,u,e,gamma,qraux,work,tol,dd,tdeg,cdeg,s)
      integer column,d,dd,execnt,i,i3,i9,info,inorm2,j,jj,jpvt,k,kernel,
     $n,nf,od,sing,tdeg
      integer cdeg(8),psi(n)
      real machep,f,i1,i10,i2,i4,i5,i6,i7,i8,rcond,rho,scal,tol
      real g(15),sigma(15),u(15,15),e(15,15),b(nf,k),colnor(15),dist(n),
     $eta(nf),gamma(15),q(d),qraux(15),rw(n),s(0:od),w(nf),work(15),x(n,
     $d),y(n)
      external ehg106,ehg182,ehg184,sqrdc,sqrsl,ssvdc
      integer isamax
      external isamax
      real r1mach
      external r1mach
      real sdot
      external sdot
      save machep,execnt
      data execnt /0/
c     colnorm -> colnor
c     E -> g
c     MachEps -> machep
c     V -> e
c     X -> b
      execnt=execnt+1
      if(execnt.eq.1)then
         machep=r1mach(4)
      end if
c     sort by distance
      do 3 i3=1,n
         dist(i3)=0
    3 continue
      do 4 j=1,dd
         i4=q(j)
         do 5 i3=1,n
            dist(i3)=dist(i3)+(x(i3,j)-i4)**2
    5    continue
    4 continue
      call ehg106(1,n,nf,1,dist,psi,n)
      rho=dist(psi(nf))*max(1.,f)
      if(.not.(0.lt.rho))then
         call ehg182(120)
      end if
c     compute neighborhood weights
      if(kernel.eq.2)then
         do 6 i=1,nf
            if(dist(psi(i)).lt.rho)then
               i1=sqrt(rw(psi(i)))
            else
               i1=0
            end if
            w(i)=i1
    6    continue
      else
         do 7 i3=1,nf
            w(i3)=sqrt(dist(psi(i3))/rho)
    7    continue
         do 8 i3=1,nf
            w(i3)=sqrt(rw(psi(i3))*(1-w(i3)**3)**3)
    8    continue
      end if
      if(abs(w(isamax(nf,w,1))).eq.0)then
         call ehg184('at ',q,dd,1)
         call ehg184('radius ',rho,1,1)
         if(.not..false.)then
            call ehg182(121)
         end if
      end if
c     fill design matrix
      column=1
      do 9 i3=1,nf
         b(i3,column)=w(i3)
    9 continue
      if(tdeg.ge.1)then
         do 10 j=1,d
            if(cdeg(j).ge.1)then
               column=column+1
               i5=q(j)
               do 11 i3=1,nf
                  b(i3,column)=w(i3)*(x(psi(i3),j)-i5)
   11          continue
            end if
   10    continue
      end if
      if(tdeg.ge.2)then
         do 12 j=1,d
            if(cdeg(j).ge.1)then
               if(cdeg(j).ge.2)then
                  column=column+1
                  i6=q(j)
                  do 13 i3=1,nf
                     b(i3,column)=w(i3)*(x(psi(i3),j)-i6)**2
   13             continue
               end if
               do 14 jj=j+1,d
                  if(cdeg(jj).ge.1)then
                     column=column+1
                     i7=q(j)
                     i8=q(jj)
                     do 15 i3=1,nf
                        b(i3,column)=w(i3)*(x(psi(i3),j)-i7)*(x(psi(i3),
     $jj)-i8)
   15                continue
                  end if
   14          continue
            end if
   12    continue
         k=column
      end if
      do 16 i3=1,nf
         eta(i3)=w(i3)*y(psi(i3))
   16 continue
c     equilibrate columns
      do 17 j=1,k
         scal=0
         do 18 inorm2=1,nf
            scal=scal+b(inorm2,j)**2
   18    continue
         scal=sqrt(scal)
         if(0.lt.scal)then
            do 19 i3=1,nf
               b(i3,j)=b(i3,j)/scal
   19       continue
            colnor(j)=scal
         else
            colnor(j)=1
         end if
   17 continue
c     singular value decomposition
      call sqrdc(b,nf,nf,k,qraux,jpvt,work,0)
      call sqrsl(b,nf,nf,k,qraux,eta,work,eta,eta,work,work,1000,info)
      do 20 i9=1,k
         do 21 i3=1,k
            u(i3,i9)=0
   21    continue
   20 continue
      do 22 i=1,k
         do 23 j=i,k
            u(i,j)=b(i,j)
   23    continue
   22 continue
      call ssvdc(u,15,k,k,sigma,g,u,15,e,15,work,21,info)
      if(.not.(info.eq.0))then
         call ehg182(182)
      end if
      tol=sigma(1)*(100*machep)
      rcond=min(rcond,sigma(k)/sigma(1))
      if(sigma(k).le.tol)then
         sing=sing+1
         if(sing.eq.1)then
            call ehg184('Warning. pseudoinverse used at',q,d,1)
            call ehg184('neighborhood radius',sqrt(rho),1,1)
            call ehg184('reciprocal condition number ',rcond,1,1)
         else
            if(sing.eq.2)then
               call ehg184('There are other near singularities as well.'
     $,rho,1,1)
            end if
         end if
      end if
c     compensate for equilibration
      do 24 j=1,k
         i10=colnor(j)
         do 25 i3=1,k
            e(j,i3)=e(j,i3)/i10
   25    continue
   24 continue
c     solve least squares problem
      do 26 j=1,k
         if(tol.lt.sigma(j))then
            i2=sdot(k,u(1,j),1,eta,1)/sigma(j)
         else
            i2=0.
         end if
         gamma(j)=i2
   26 continue
      do 27 j=0,od
c        bug fix 2006-07-04 for k=1, od>1.   (thanks btyner@gmail.com)
         if(j.lt.k)then
            s(j)=sdot(k,e(j+1,1),15,gamma,1)
         else
            s(j)=0.
         end if
   27 continue
      return
      end
