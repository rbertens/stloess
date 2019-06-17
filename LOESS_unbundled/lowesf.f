      subroutine lowesf(xx,yy,ww,iv,liv,lv,wv,m,z,l,ihat,s)
      logical i1
      integer execnt,ihat,m,n
      integer iv(*)
      real l(m,*),s(m),wv(*),ww(*),xx(*),yy(*),z(m,1)
      external ehg182,ehg136
      save execnt
      data execnt /0/
      execnt=execnt+1
      if(171.le.iv(28))then
         i1=(iv(28).le.174)
      else
         i1=.false.
      end if
      if(.not.i1)then
         call ehg182(171)
      end if
      iv(28)=172
      if(.not.(iv(14).ge.iv(19)))then
         call ehg182(186)
      end if
      call ehg136(z,m,m,iv(3),iv(2),iv(19),wv(1),xx,iv(iv(22)),yy,ww,iv(
     $20),iv(29),wv(iv(15)),wv(iv(16)),wv(iv(18)),0,l,ihat,wv(iv(26)),wv
     $(4),iv(30),iv(33),iv(32),iv(41),s)
      return
      end
