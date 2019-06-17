      subroutine loread(iunit,d,vc,nc,nv,iv,liv,lv,v)
      integer bound,d,execnt,iunit,liv,lv,nc,nv,vc
      integer iv(liv)
      real v(lv)
      external ehg168,ehg169,ehg182
      save execnt
      data execnt /0/
      execnt=execnt+1
      iv(28)=173
      iv(2)=d
      iv(4)=vc
      iv(14)=nv
      iv(17)=nc
      iv(7)=50
      iv(8)=iv(7)+nc
      iv(9)=iv(8)+vc*nc
      iv(10)=iv(9)+nc
      bound=iv(10)+nc
      if(.not.(bound-1.le.liv))then
         call ehg182(102)
      end if
      iv(11)=50
      iv(13)=iv(11)+nv*d
      iv(12)=iv(13)+(d+1)*nv
      bound=iv(12)+nc
      if(.not.(bound-1.le.lv))then
         call ehg182(103)
      end if
      call ehg168(iunit,d,vc,nc,nv,nv,v(iv(11)),iv(iv(7)),v(iv(12)),v(iv
     $(13)))
      call ehg169(d,vc,nc,nc,nv,nv,v(iv(11)),iv(iv(7)),v(iv(12)),iv(iv(8
     $)),iv(iv(9)),iv(iv(10)))
      return
      end
