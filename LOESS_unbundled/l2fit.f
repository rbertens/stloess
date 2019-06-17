      subroutine ehg136(u,lm,m,n,d,nf,f,x,psi,y,rw,kernel,k,dist,eta,b,o
     $d,o,ihat,w,rcond,sing,dd,tdeg,cdeg,s)
      integer identi,d,dd,execnt,i,i1,ihat,info,j,k,kernel,l,lm,m,n,nf,o
     $d,sing,tdeg
      integer cdeg(8),psi(n)
      real f,i2,rcond,scale,tol
      real o(m,n),sigma(15),e(15,15),g(15,15),b(nf,k),dist(n),eta(nf),ga
     $mma(15),q(8),qraux(15),rw(n),s(0:od,m),u(lm,d),w(nf),work(15),x(n,
     $d),y(n)
      external ehg127,ehg182,sqrsl
      real sdot
      external sdot
      save execnt
      data execnt /0/
c     V -> g
c     U -> e
c     Identity -> identi
c     L -> o
c     X -> b
      execnt=execnt+1
      if(.not.(k.le.nf-1))then
         call ehg182(104)
      end if
      if(.not.(k.le.15))then
         call ehg182(105)
      end if
      do 3 identi=1,n
         psi(identi)=identi
    3 continue
      do 4 l=1,m
         do 5 i1=1,d
            q(i1)=u(l,i1)
    5    continue
         call ehg127(q,n,d,nf,f,x,psi,y,rw,kernel,k,dist,eta,b,od,w,rcon
     $d,sing,sigma,e,g,gamma,qraux,work,tol,dd,tdeg,cdeg,s(0,l))
         if(ihat.eq.1)then
c           $L sub {l,l} =
c           V sub {1,:} SIGMA sup {+} U sup T
c           (Q sup T W e sub i )$
            if(.not.(m.eq.n))then
               call ehg182(123)
            end if
c           find $i$ such that $l = psi sub i$
            i=1
c           top of while loop
    6       if(.not.(l.ne.psi(i)))goto 7
               i=i+1
               if(.not.(i.lt.nf))then
                  call ehg182(123)
               end if
               goto 6
c           bottom of while loop
    7       do 8 i1=1,nf
               eta(i1)=0
    8       continue
            eta(i)=w(i)
c           $eta = Q sup T W e sub i$
            call sqrsl(b,nf,nf,k,qraux,eta,eta,eta,eta,eta,eta,1000,info
     $)
c           $gamma = U sup T eta sub {1:k}$
            do 9 i1=1,k
               gamma(i1)=0
    9       continue
            do 10 j=1,k
               i2=eta(j)
               do 11 i1=1,k
                  gamma(i1)=gamma(i1)+i2*e(j,i1)
   11          continue
   10       continue
c           $gamma = SIGMA sup {+} gamma$
            do 12 j=1,k
               if(tol.lt.sigma(j))then
                  gamma(j)=gamma(j)/sigma(j)
               else
                  gamma(j)=0.
               end if
   12       continue
c           voidp junk
c           voidp junk
            o(l,1)=sdot(k,g(1,1),15,gamma,1)
         else
            if(ihat.eq.2)then
c              $L sub {l,:} =
c              V sub {1,:} SIGMA sup {+}
c              ( U sup T Q sup T ) W $
               do 13 i1=1,n
                  o(l,i1)=0
   13          continue
               do 14 j=1,k
                  do 15 i1=1,nf
                     eta(i1)=0
   15             continue
                  do 16 i1=1,k
                     eta(i1)=e(i1,j)
   16             continue
                  call sqrsl(b,nf,nf,k,qraux,eta,eta,work,work,work,work
     $,10000,info)
                  if(tol.lt.sigma(j))then
                     scale=1./sigma(j)
                  else
                     scale=0.
                  end if
                  do 17 i1=1,nf
                     eta(i1)=eta(i1)*(scale*w(i1))
   17             continue
                  do 18 i=1,nf
                     o(l,psi(i))=o(l,psi(i))+g(1,j)*eta(i)
   18             continue
   14          continue
            end if
         end if
    4 continue
      return
      end
