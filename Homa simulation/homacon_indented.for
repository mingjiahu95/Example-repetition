c
c      homacon_indented.for
c      constrain the low, med, and high values as proportional to the 
c       objective average dot displacements
c
c      indent the do-loops to make it easier for Mingjia to read
c
       subroutine model(is1,nsim,ndim,abase,theory)
c
c      array type and size declarations
c
       double precision abase(50),theory(3,2,4)
       double precision pran,z,sum(3),s,d,val(3)
       double precision prot(3,9),train(2,3,100,9)
       double precision testpat(4,9),foil(9)
       double precision class(2,4),rec1(2,4),rec2(2,4)
       double precision crit(3,2)
       integer ntrain(2)
       real low,med,high,nrep(2)
c
c      experiment design settings
c
       ntrain(1)=5
       ntrain(2)=100
       nrep(1)=20
       nrep(2)=1
c
c      parameter value assignments
c
       between=abase(1)
       scale=abase(2)
       low=scale*1.20
       med=scale*2.80
       high=scale*4.60
       c=abase(3)
       back=abase(4)
       gam=abase(5)
       crit(2,1)=abase(6)
       crit(2,2)=abase(7)
       crit(3,1)=abase(8)
       crit(3,2)=abase(9)
c
c      initialize theoretical-prediction array
c
       do 10 iexp=1,3
       do 10 icond=1,2
       do 10 itype=1,4
         theory(iexp,icond,itype)=0
10     continue
c
c      conduct nsim simulations and aggregate the results
c
       do 5000 isim=1,nsim
c
c        construct the prototypes for this simulation
c
         do 21 icat=1,3
         do 20 m=1,ndim
           call rand(is1,pran)
           prot(icat,m)=between*pran
20       continue
21       continue
c
c        construct the training patterns for each condition
c         and category for this simulation
c
         do 50 icond=1,2
         do 30 icat=1,3
         do 25 ipat=1,ntrain(icond)
         do 22 m=1,ndim
           call rand(is1,pran)
           call zscor(pran,z)
           train(icond,icat,ipat,m)=prot(icat,m)+med*z
22       continue
25       continue
30       continue
50       continue
c
c        construct the various test patterns to be
c        used in this classification simulation; one pattern per each type
c
c        prototype test pattern
c
         do 61 m=1,ndim
61         testpat(1,m)=prot(1,m)
c
c        novel low distortion        
c
         do 62 m=1,ndim
           call rand(is1,pran)
           call zscor(pran,z)
62         testpat(2,m)=prot(1,m)+low*z
c
c        novel medium distortion
c
         do 63 m=1,ndim
           call rand(is1,pran)
           call zscor(pran,z)
63         testpat(3,m)=prot(1,m)+med*z
c
c        novel high distortion
c
         do 64 m=1,ndim
           call rand(is1,pran)
           call zscor(pran,z)
64         testpat(4,m)=prot(1,m)+high*z
c
c        foil pattern (to be used for recognition Exp. 2)
c
         do 65 m=1,ndim
           call rand(is1,pran)
           protval=between*pran
           call rand(is1,pran)
           call zscor(pran,z)
           foil(m)=protval+med*z
65       continue
c
c        predict classification transfer data
c
         do 500 icond=1,2
c
          do 400 itype=1,4
             do 350 icat=1,3
              sum(icat)=0
              do 300 j=1,ntrain(icond)
               d=0
               do 250 m=1,ndim
250              d=d+(testpat(itype,m)-train(icond,icat,j,m))**2
               s=dexp(-c*d)
               sum(icat)=sum(icat)+s
300           continue
350          continue
             do 351 icat=1,3
              val(icat)=(nrep(icond)*sum(icat)+back)**gam
351          continue
             class(icond,itype)=val(1)/(val(1)+val(2)+val(3))
400       continue
c
500      continue
c
c        predict recognition data for Exp. 2
c       
c        construct novel medium distortion for recognition
c        note to self:  I could also have used the novel medium
c         distortion that I had used previously for classification,
c         but the test patterns were numbered differently
c        to check program logic, set testpat2 = testpat3 from previous
c
         do 460 m=1,ndim
           call rand(is1,pran)
           call zscor(pran,z)
460        testpat(2,m)=prot(1,m)+med*z
c
c        set testpat3 to the foil values constructed earlier
c
         do 461 m=1,ndim
461         testpat(3,m)=foil(m)
c
         do 2000 icond=1,2
c
c        compute recognition probabilities for each of conditions 1 and 2
c          in Experiment 2
c
c          set testpat1 equal to the first training example of the first category
c          for condition icond
c
           do 462 m=1,ndim
462          testpat(1,m)=train(icond,1,1,m)
c
c          now compute the summed similarities and
c          recognition probabilities for each of the
c          three item types
c
  	   do 1400 itype=1,3
             jcount=0
             sum(1)=0
             do 1350 icat=1,3
             do 1350 j=1,ntrain(icond)
               d=0
               do  1250 m=1,ndim
1250             d=d+(testpat(itype,m)-train(icond,icat,j,m))**2
               s=dexp(-c*d)        
1275           sum(1)=sum(1)+s
1350         continue
             xnum=(nrep(icond)*sum(1)+back)**gam
             rec1(icond,itype)=xnum/(xnum+crit(2,icond))
1400       continue
2000     continue
c      
c
c        predict recognition for Experiment 3
c
c         testpat(1) and testpat(2) stay the same as above
c          but now testpat3 is the Cat-1 prototype
c
         do 2460 m=1,ndim
2460       testpat(3,m)=prot(1,m)
c
c
c
         do 3000 icond=1,2
c
c        compute recognition probabilities for each of conditions 1 and 2
c          in Experiment 3
c
c           set testpat1 equal to the first training example of the first category
c           for condition icond         
c
           do 463 m=1,ndim
463         testpat(1,m)=train(icond,1,1,m)
c
c          now compute the summed similarities and
c          recognition probabilities for each of the
c          three item types
c
           do 2400 itype=1,3
             sum(1)=0
             do 2350 icat=1,3
             do 2350 j=1,ntrain(icond)
               d=0
               do  2250 m=1,ndim
2250             d=d+(testpat(itype,m)-train(icond,icat,j,m))**2
               s=dexp(-c*d)
               sum(1)=sum(1)+s
2350         continue
             xnum=(nrep(icond)*sum(1)+back)**gam
             rec2(icond,itype)=xnum/(xnum+crit(3,icond))
2400       continue
3000     continue
c
c        aggregate this simulation's predictions
c         into the grand average predictions
c
         do 4500 icond=1,2
         do 4500 itype=1,4
         theory(1,icond,itype)=theory(1,icond,itype)+
     .              class(icond,itype)/float(nsim)
4500     continue
         do 4600 icond=1,2
         do 4600 itype=1,3
         theory(2,icond,itype)=theory(2,icond,itype)+
     .              rec1(icond,itype)/float(nsim)
         theory(3,icond,itype)=theory(3,icond,itype)+
     .              rec2(icond,itype)/float(nsim)
4600     continue
c
5000   continue
c
c      end of simulations, model predictions are in theory array
c     
       return
       end
