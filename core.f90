!COPYRIGHT
!WORLD COMPANY IASIG 2013
!GSE - OTH - PFO



!***********************************************************************************************************!
!***********************************************************************************************************!
!***********************************************************************************************************!
!MODULE TABLES_DE_GRAPHE
! ce module contient seulement la description des tables exprimant la topologie d'un graphe.
!SIF, SXY, PAXY
!   -NUMRECORD, retourne le nombre d'enregistrements des differents fichiers
!   -READFILE, lit les fichiers et retourne un tableau de donnees, ainsi que le nombre d'enregistrements
!   -SPLIT, ce charge de separer une ligne du fichier en element indice.
!***********************************************************************************************************!

module tables_de_graphe

implicit none

!***********************************************************************************************************!
!Declaration
   integer                               :: NA, NS, ND,NAXY
   integer, dimension (:,:), allocatable :: SIF, GD
   !REM: SIF, SXY, etc ne doivent jamais être triées de quelques façon que ce soit
   integer, dimension (:,:), allocatable :: SXY, AXY, PAXY
   !addition PFO
   !Declaration Parametres formels pour Module Parcours de Graphe
   integer, allocatable, dimension (:,:) :: VectorTable
   real :: pi=3.14159


end module tables_de_graphe
!***********************************************************************************************************!



!***********************************************************************************************************!
!***********************************************************************************************************!
!***********************************************************************************************************!
!MODULE LIRE_DONNEES
!Ce module contient les fonctions necessaires pour recuperer les donnees stockees dans les tables:
!SIF, SXY, PAXY
!   -NUMRECORD, retourne le nombre d'enregistrements des differents fichiers
!   -READFILE, lit les fichiers et retourne un tableau de donnees, ainsi que le nombre d'enregistrements
!   -SPLIT, ce charge de separer une ligne du fichier en element indice.
!***********************************************************************************************************!

module lire_donnees

implicit none
contains

    !***********************************************************************************************************!
    !***********************************************************************************************************!
    !SUBROUTINE
    subroutine READFILE(pathFile, tab, n)
    !READFILE, lit les fichiers et retourne un tableau de donnees, ainsi que le nombre d'enregistrements
    !   -pathFile :: chemins d'acces au fichier
    !   -tab :: tableau d'element retourne par le fichier
    !   -n :: nombre d'elements contenu dans le tableau
    !***********************************************************************************************************!
    !Specification

        character(200), intent(in)      ::pathFile
        integer,  intent(out),allocatable, dimension(:,:)      ::tab
        integer, intent(in)             ::n

    !***********************************************************************************************************!
    !Declaration
        logical                         ::exist
        integer                         ::io,  i, j, nelmt
        character(20)                   ::str
        character(1)                    ::sep
        integer, dimension(3)           ::elmt

    !***********************************************************************************************************!
    !Body
        allocate(tab(n,2))
        !Verification de l'existence du fichier
        inquire(file=pathFile,exist= exist)
        if (.NOT. exist) then
            print *, "Erreur! Fichier non existant"
            return
        endif

        !Ouverture du fichier
        open(10, file=pathFile)
        !Lecture du fichier sur n lignes (on les a recupere avec NUMRECORD
        do i = 1, n
            read(10,1000,iostat=io)str
            !Test de fin de fichier
            if(io < 0) exit
            sep = ";"
            !Recuperation des elements contenus dans la chaene
            call SPLIT(str, sep, elmt, nelmt)

            do j = 1, nelmt
                tab(i,j) = elmt(j)
                end do
        enddo
        !Fermeture du fichier
        close(10)


    1000 FORMAT(a10)
    end subroutine READFILE

    !***********************************************************************************************************!
    !***********************************************************************************************************!
    !SUBROUTINE
    subroutine NUMRECORD(pathFile, n)
    !READFILE, lit les fichiers et retourne un tableau de donnees, ainsi que le nombre d'enregistrements

    !***********************************************************************************************************!
    !Specification

        character(200), intent(in)      ::pathFile
        integer, intent(out)            ::n

    !***********************************************************************************************************!
    !Declaration
        logical                         ::exist
        integer                         ::io

        n = 0
    !***********************************************************************************************************!
    !Body
        inquire(file=pathFile,exist= exist)
        if (.NOT. exist) then
            print *, "Erreur! Fichier non existant"
            return
        endif

        !Ouverture du fichier
        open(10, file=pathFile)
        !Lecture du fichier
        do
            read(10,*,iostat=io)
            !Test si fin du fichier
            if(io < 0) exit
            n = n + 1

        enddo
        !Fermeture du fichier
        close(10)

    end subroutine NUMRECORD


    !***********************************************************************************************************!
    !-----------------------------
    !SUBROUTINE
    subroutine SPLIT(str, sep, T, n)
    !SPLIT, ce charge de separer une ligne du fichier en element indice.
    !   -str :: la chaene de caractere e traiter
    !   -sep :: le separateur d'elements
    !   - T :: le tableau d'element qui sera retourne. La taille peut-etre variable (XY ou XYZ, etc.)
    !   - n :: le nombred'element dans le tabeau.

    !-----------------------------
    !Specification
        character(20), intent(in)       ::str
        character(1), intent(in)       ::sep
        integer, dimension(:), intent(out) ::T
        integer, intent(out)            ::n

    !-----------------------------
    !Declaration
        integer             :: pos1, pos2, val

    !-----------------------------
    !Body
        pos1 = 1
        n = 0

       do
            pos2 = index( str(pos1:len_trim(str)),sep)
            if (pos2 == 0) then
                n = n + 1
                !Convertir la chaine de caractere en entier
                read(str(pos1:), *) val
                T(n) = val
                exit
            end if
            n = n + 1
            !Convertir la chaine de caractere en entier
            read(str(pos1:pos1 + pos2 - 2), *) val
            T(n) = val
            pos1 = pos1 + pos2
        end do

    end subroutine SPLIT

end module lire_donnees



!***********************************************************************************************************!
!***********************************************************************************************************!
!***********************************************************************************************************!
!MODULE TOPOLOGIE
! ce module contient les procedures de base operant sur la topologie d'un graphe :
!   - ESS , entrants et sortants d'un sommet
!   - PSS , predecesseurs et successeursq d'un sommet
!   - PSA , predecesseurs et successeurs d'un arc
!   - DIRETRO , arcs directs et retrogrades d'un domaine
!   - ADJ , arcs adjacents a un domaine
!   - PERIPH, sommets peripheriques d'un domaine
!   - ENTOUR, domaines entourant un sommet
!       ces deux derniers modules utilisent la procedure
!       PERI_ENTOUR , donnant :
!             -soit les sommets peripheriques d'un domaine
!             -soit les domaines entourant un sommet
!   - OPP , domaines opposes par un sommet a un domaine
!***********************************************************************************************************!
module topologie

!***********************************************************************************************************!
!Declaration
use tables_de_graphe

implicit none
           private PERI_ENTOUR
                                                              !
   contains                                                   !
                                                              !
   !===========================================================
   subroutine ESS (s,  TA, nta, k )                           !
   !----------------------------------------------------------!
   !    donne pour un sommet s, dans un vecteur TA,           !
   !    de cardinalit� nta, la liste des arcs :               !
   !       -sortant de s, pour k=1                            !
   !       -entrant dans s, pour k=2                          !
   !----------------------------------------------------------!
   !                       specifications                     !
      integer, intent (in)                   :: s             !
      integer, intent (out),dimension (1:NA) :: TA            !
      integer, intent (out)                  :: nta           !
      integer, intent (in)                   :: k             !
   !----------------------------------------------------------!
   !                        declarations                      !
      integer ::  i                                           !
   !----------------------------------------------------------!
   !                           corps                          !
     nta = 0                                                  !
      do i = 1, NA                                            !
         if ( SIF(i,k) /= s ) then                            !
             cycle                                            !
         else                                                 !
         endif                                                !
         nta = nta + 1 ; TA (nta)= i                          !
      enddo !--i--                                            !
   !----------------------------------------------------------!
   end subroutine !--ESS--                                    !
   !===========================================================

   !===========================================================
   subroutine PSS (s, TS, nts, k )                            !
   !----------------------------------------------------------!
   !   donne pour le sommet s, dans un vecteur TS,            !
   !   de cardinalit� nts, la liste :                         !
   !      -des sommets successeurs, pour k=1                  !
   !      -des sommets pr�d�cesseurs, pour k=2                !
   !----------------------------------------------------------!
   !                       specifications                     !
      integer, intent (in)                   :: s                 !
      integer, intent (out),dimension (1:NA) :: TS            !
      integer, intent (out)                  :: nts           !
      integer, intent (in)                   :: k             !
   !----------------------------------------------------------!
   !                        declarations                      !
      integer ::  i, j                                        !
   !----------------------------------------------------------!
   !                           corps                          !
    nts = 0                                                   !
    bou: do i = 1, NA ;                                       !
           if ( SIF(i,k) /= s ) then                          !
              cycle                                           !
           else                                               !
           endif                                              !
           if ( nts /= 0 ) then                               !
              do j = 1 , nts                                  !
                 if ( TS(j) == SIF(i,3-k)) then               !
                    cycle bou                                 !
                 else                                         !
                 endif                                        !
              enddo !--j--                                    !
           else                                               !
           endif                                              !
                                                              !
           nts = nts + 1 ; TS(nts)=SIF(i, 3-k)                !
         enddo bou !--i--                                     !
   !----------------------------------------------------------!
   end subroutine !--PSS--                                    !
   !===========================================================

   !===========================================================
   subroutine PSA (a, TA, nta, k )                            !
   !----------------------------------------------------------!
   !   donne pour l'arc a, dans une vecteur TA,               !
   !   de cardinalit� nta, la liste des arcs :                !
   !       -les successeurs de a, lorsque k=2                 !
   !       -les predecesseurs de a, lorsque k=1               !
   !----------------------------------------------------------!
   !                       specifications                     !
      integer, intent (in)                   :: a             !
      integer, intent (out),dimension (1:NA) :: TA            !
      integer, intent (out)                  :: nta           !
      integer, intent (in)                   :: k             !
   !----------------------------------------------------------!
   !                        declarations                      !
      integer ::  i                                           !
   !----------------------------------------------------------!
   !                           corps                          !
     nta = 0                                                  !
      do i = 1, NA                                            !
         if ( SIF(i,3-k) /= SIF(a,k) ) then                   !
             cycle                                            !
         else                                                 !
         endif                                                !
         nta = nta + 1 ; TA (nta)= i                          !
      enddo !--i--                                            !
   !----------------------------------------------------------!
   end subroutine !--PSA--                                    !
   !===========================================================

   !===========================================================
   subroutine DIRETRO ( d , TA, nta, k )                      !
   !----------------------------------------------------------!
   !   donne pour le domaine d , dans le vecteur TA,          !
   !   de cardinalit� nta,                                    !
   !      -les arcs directs, lorsque k=1                      !
   !      -les arcs r�trogrades, lorsque k=2                  !
   !----------------------------------------------------------!
   !                       specifications                     !
      integer, intent (in)                   :: d             !
      integer, intent (out),dimension (1:10) :: TA            !
      integer, intent (out)                  :: nta           !
      integer, intent (in)                   :: k             !
   !----------------------------------------------------------!
   !                        declarations                      !
      integer ::  i                                           !
   !----------------------------------------------------------!
   !                           corps                          !
     nta = 0                                                  !
      do i = 1, NA                                            !
         if ( GD(i,k) /= d ) then                             !
             cycle                                            !
         else                                                 !
         endif                                                !
         nta = nta + 1 ; TA (nta)= i                          !
      enddo !--i--                                            !
   !----------------------------------------------------------!
   end subroutine !--DIRETRO--                                !
   !===========================================================

   !===========================================================
   subroutine ADJ (d, TD, ntd )                               !
   !----------------------------------------------------------!
   !   donne pour le domaine d , dans le vecteur TD,          !
   !   de cardinalit� ntd,les domaines adjacents � d.         !                           !
   !----------------------------------------------------------!
   !                       specifications                     !
      integer, intent (in)                   :: d             !
      integer, intent (out),dimension (1:10) :: TD            !
      integer, intent (out)                  :: ntd           !
   !----------------------------------------------------------!
   !                        declarations                      !
      integer ::  i, j, k                                     !
   !----------------------------------------------------------!
   !                           corps                          !
     ntd = 0                                                  !
     do i = 1, NA                                             !
       bou: do j = 1, 2                                       !
               if ( GD(i,j) /= d ) then                       !
                  cycle                                       !
               else                                           !
               endif                                          !
               if ( ntd /= 0 ) then                           !
                  do k = 1, ntd                               !
                     if (TD(k) == GD(i, 3-j) ) then           !
                        cycle bou    ! next 2                 !
                     else                                     !
                     endif                                    !
                  enddo !--k--                                !
               else                                           !
               endif                                          !
               ntd = ntd + 1 ; TD (ntd)= GD(i, 3-j)           !
            enddo bou  !--j--                                 !
       enddo !--i--                                           !
   !----------------------------------------------------------!
   end subroutine ADJ                                         !
   !===========================================================

   !===========================================================
   subroutine PERIPH (d, T, nt )                              !
   !----------------------------------------------------------!
   ! donne dans le vecteur T, de cardinalite nt, la liste des !
   ! sommets peripheriques du domaine d                       !
   !----------------------------------------------------------!
   !                       specifications                     !
     integer, intent (in)                   :: d              !
     integer, intent (out),dimension(1:10)  :: T              !
     integer, intent (out)                  :: nt             !
   !----------------------------------------------------------!
   !                        declarations                      !
   !----------------------------------------------------------!
   !                          externes                        !
!     interface                                               !
       !.................................................     !
!      subroutine PERI_ENTOUR (c, A, B, T, nt )               !
!         integer, intent (in)                   :: c         !
!         integer, intent (in), dimension(:,:)   :: A,B       !
!         integer, intent (out),dimension(1:10)  :: T         !
!         integer, intent (out)                  :: nt        !
!      end subroutine ! -- PERI_ENTOUR --                     !
       !..................................................    !
!    end interface                                            !
   !----------------------------------------------------------!
   !                           corps                          !
        call PERI_ENTOUR (d, GD, SIF, T, nt )                 !
   !----------------------------------------------------------!
   end subroutine ! -- PERIPH --                              !
   !===========================================================

   !===========================================================
   subroutine ENTOUR (s, T, nt )                              !
   !----------------------------------------------------------!
   ! donne dans le vecteur T, de cardinalite nt, la liste des !
   ! domaines entourant le sommet s                           !
   !----------------------------------------------------------!
   !                       specifications                     !
     integer, intent (in)                   :: s              !
     integer, intent (out),dimension(1:10)  :: T              !
     integer, intent (out)                  :: nt             !
   !----------------------------------------------------------!
   !                        declarations                      !
   !----------------------------------------------------------!
   !                          externes                        !
!     interface                                               !
       !.................................................     !
!      subroutine PERI_ENTOUR (c, A, B, T, nt )               !
!         integer, intent (in)                   :: c         !
!         integer, intent (in), dimension(:,:)   :: A,B       !
!         integer, intent (out),dimension(1:10)  :: T         !
!          integer, intent (out)                 :: nt        !
!      end subroutine ! -- PERI_ENTOUR --                     !
       !..................................................    !
!    end interface                                            !
   !----------------------------------------------------------!
   !                           corps                          !
        call PERI_ENTOUR (s, SIF, GD, T, nt )                 !
   !----------------------------------------------------------!
   end subroutine !-- ENTOUR --                               !
   !===========================================================

   !===========================================================
   subroutine PERI_ENTOUR (c, A, B, T, nt )                   !
   !----------------------------------------------------------!
   !  donne pour le composant c, un vecteur de cardinalite nt.!
   !    - c : domaine, T contient les sommets peripheriques   !
   !    - c : sommet, T contient les domaines entourants.     !
   !----------------------------------------------------------!
   !                       specifications                     !
     integer, intent (in)                   :: c              !
     integer, intent (in), dimension(:,:)   :: A,B            !
     integer, intent (out),dimension(1:10)  :: T              !
     integer, intent (out)                  :: nt             !
   !----------------------------------------------------------!
   !                        declarations                      !
      integer    :: i, j, k, h                                !
   !----------------------------------------------------------!
      nt = 0                                                  !
      do i = 1, NA                                            !
         do j = 1, 2                                          !
            if ( A(i,j) /= c ) then                           !
               cycle                                          !
            else                                              !
            endif                                             !
            bou : do k = 1, 2                                 !
                     if ( nt /= 0 ) then                      !
                        do h = 1, nt                          !
                           if ( T(h) == B(i,k) ) then         !
                               cycle bou       ! next 2       !
                           else                               !
                           endif                              !
                        enddo  !--h-- ;                       !
                     else                                     !
                     endif                                    !
                     nt = nt + 1 ; T(nt) = B(i,k)             !
                   enddo bou  !--k--                          !
          enddo !--j--                                        !
      enddo !--i--                                            !
   !----------------------------------------------------------!
   end subroutine ! -- PERI_ENTOUR --                         !
   !===========================================================

   !===========================================================
   subroutine OPP (d, T, nt )                                 !
   !----------------------------------------------------------!
   !   donne pour le domaine d , dans le vecteur T ,          !
   !   de cardinalit� nt ,les domaines opposes � d.           !
   !----------------------------------------------------------!
   !                       specifications                     !
      integer, intent (in)                   :: d             !
      integer, intent (out),dimension (1:10) :: T             !
      integer, intent (out)                  :: nt            !
   !----------------------------------------------------------!
   !                        declarations                      !
      integer                    :: nl,i,j,k,h,a,s,dd         !
      integer, dimension (1:100) ::  L                        !
   !----------------------------------------------------------!
   !                           corps                          !
     nt  = 0                                                  !
     do i = 1, NA                                             !
       do j = 1, 2                                            !
         if ( GD(i,j) /= d ) then                             !
            cycle                                             !
         else                                                 !
         endif                                                !
         nt = nt + 1 ; T(nt) = i                              !
       enddo !--j--                                           !
     enddo !--i--                                             !
                                                              !
   ! on obtient ici tous les arcs, directs ou retrogrades,    !
   ! entourant le domaine d, et ceci sans duplicata           !
     nl = 0                                                   !
     do i = 1, nt                                             !
        a = T (i)                                             !
        bou_1 : do j = 1, 2 ;                                 !
                   if ( nl /= 0 ) then                        !
                      do  k = 1, nl                           !
                         if ( L(k) == SIF(a,j) ) then         !
                             cycle bou_1                      !
                         else                                 !
                         endif                                !
                      enddo !--k--                            !
                   else                                       !
                   endif                                      !
                   nl = nl + 1 ; L (nl) =  SIF (a, j)         !
                 enddo bou_1  !--j--                          !
      enddo !--i--                                            !
                                                              !
    ! on a  les sommets peripheriques de d sans duplicata     !
                                                              !
      nt = 0 ;                                                !
      do i = 1 , nl                                           !
         s = L(i)                                             !
         do j = 1, NA                                         !
           bou_2 : do k = 1, 2                                !
                     if ( SIF(j,k) /= s ) then                !
                         cycle                                !
                     else                                     !
                     endif                                    !
                     if ( nt /= 0 ) then                      !
                        do h = 1, nt ;                        !
                           if ( T(h) == j ) then              !
                              cycle bou_2                     !
                           else                               !
                           endif                              !
                        enddo !--h--                          !
                     else                                     !
                     endif                                    !
                     nt = nt + 1 ; T(nt) = j                  !
                    enddo bou_2   !--k--                      !
          enddo !--j--                                        !
      enddo !--i--                                            !
                                                              !
    ! on a tous les arcs entrants et sortants sans duplicata  !
                                                              !
      nl = 0                                                  !
      do i = 1, nt                                            !
         a = T(i)                                             !
         bou_3 : do j = 1,2                                   !
                    if ( nl /= 0 ) then                       !
                       do k = 1, nl                           !
                          if ( L(k) == GD(a,j) ) then         !
                             cycle  bou_3                     !
                          else                                !
                          endif                               !
                       enddo !--k--                           !
                     else                                     !
                     endif                                    !
                     nl = nl + 1 ; L(nl) = GD (a,j)           !
                  enddo  bou_3  !--j--                        !
       enddo !--i--                                           !
                                                              !
    !  on a a tous les domaines concernant d,sans duplicata,  !
    !  c'est a dire : - les domaines adjacents a d,           !
    !                 - d lui-meme,                           !
    !                 - ses eventuels opposes.                !
                                                              !
      nt = 0                                                  !
      bou_4: do i = 1, nl                                     !
                dd = L(i)                                     !
                if ( dd == d ) then                           !
                    cycle     !-- c'est d lui-meme --         !
                else                                          !
                endif                                         !
                do j = 1, NA                                  !
                  if( GD(i,1) == d .AND. GD(i,2) == dd .OR.&  !
                    & GD(i,1) == dd .AND. GD(i,2) == d ) then !
                     cycle  bou_4   !-- dd est adjacent a d --!
                  else                                        !
                  endif                                       !
                enddo !--j--                                  !
                             !-- donc dd est un oppose a d -- !
                nt = nt + 1 ; T(nt) = dd                      !
             enddo  bou_4  !--i--                             !
   !----------------------------------------------------------!
   end subroutine OPP                                         !
   !===========================================================
                                                              !
!-------------------------------------------------------------!
end module topologie                                          !
!MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM




module metrique

!***********************************************************************************************************!
!***********************************************************************************************************!
!***********************************************************************************************************!
! ce module contient des tables decrivant la metrique d'un
! graphe, ainsi que les procedures de base operant sur la
! metrique de ce graphe :
!      - LGR_ARC , donne la longueur d'un arc
!      - PERIM , donne le perimetre d'un domaine
!      - SURFACE , donne la surface d'un domaine
! il utilise le module "topologie"
!***********************************************************************************************************!
!***********************************************************************************************************!

!***********************************************************************************************************!                                                              !
   use  topologie                                             !
   !---------------------                                     !
implicit none                                                 !
                   private LGR_SEG                            !
   contains                                                   !
                                                              !
   !===========================================================
   real function LGR_ARC ( a ) result (lgr)                   !
   !----------------------------------------------------------!
   !                       specifications                     !
      integer, intent (in)  :: a                              !
   !----------------------------------------------------------!
   !                        declarations                      !
      integer   :: i, ini, fin                                !
      real      :: x1, x2, y1, y2                             !
   !----------------------------------------------------------!
   !                         externes                         !
!   interface                                                 !
        !.................................................    !
!       real function LGR_SEG (xi,xf,yi,yf) result (lgse)     !
!          real, intent (in) :: xi, xf, yi, yf                !
!       end function !-- LGR_SEG --                           !
        !.................................................    !
!   end interface                                             !
   !----------------------------------------------------------!
   !                           corps                          !
      lgr = 0.                                                !
      ini = SIF (a,1)                                         !
      x1 = float(SXY(ini,1)) ; y1 = float(SXY (ini,2))        !
      if ( PAXY(a,1) /= 0) then                               !
         do  i = PAXY(a,1), PAXY(a,2)                         !
             x2 = float(AXY(i,1)) ; y2 = float(AXY(i,2))      !
             lgr = lgr + LGR_SEG( x1, x2, y1, y2 )            !
             x1 = x2 ; y1 = y2                                !
         enddo  !--i--                                        !
      else                                                    !
      endif ;                                                 !
      fin = SIF(a,2)                                          !
      x2 = float(SXY(fin,1)) ; y2 = float(SXY (fin,2))        !
      lgr = lgr + LGR_SEG( x1, x2, y1, y2 )                   !
   !----------------------------------------------------------!
   end function ! -- LGR_ARC --                               !
   !===========================================================

   !===========================================================
   real function LGR_SEG (x1,x2,y1,y2) result (lgse)          !
   !----------------------------------------------------------!
   !                   specifications                         !
           real, intent (in) :: x1, x2, y1, y2                !
   !----------------------------------------------------------!
   !                    declarations                          !
       real  :: dx, dy                                        !
   !----------------------------------------------------------!
   !                        corps                             !
      dx = x2 - x1 ; dy = y2 - y1                             !
      lgse = sqrt( dx * dx + dy * dy )                        !
   !----------------------------------------------------------!
   end function !-- LGR_SEG --                                !
   !===========================================================

   !===========================================================
   real function PERIMETRE ( d )result (perim)                !
   !----------------------------------------------------------!
   !                       specifications                     !
      integer, intent (in)  :: d                              !
   !----------------------------------------------------------!
   !                        declarations                      !
      integer   :: i, j                                       !
   !----------------------------------------------------------!
   !                          externes                        !
!    interface                                                !
          !..........................................         !
!         real function LGR_ARC ( a ) result (lgr)  ;         !
!              integer, intent (in)  :: a   ;                 !
!         end function  ! -- LGR_ARC --                       !
          !..........................................         !
!    end interface                                            !
   !----------------------------------------------------------!
   !                           corps                          !
          perim = 0.                                          !
          do i = 1, NA                                        !
             do j = 1, 2                                      !
                if ( GD(i,j) /= d ) then                      !
                   cycle                                      !
                else                                          !
                endif                                         !
                perim = perim + LGR_ARC ( i )                 !
             enddo ! --j--                                    !
          enddo ! --i--                                       !
   !----------------------------------------------------------!
   end function ! -- PERIMETRE --                             !
   !===========================================================

   !===========================================================
   real function SURFACE ( d )result (surf)                   !
   !----------------------------------------------------------!
   !                       specifications                     !
      integer, intent (in)  :: d                              !
   !----------------------------------------------------------!
   !                        declarations                      !
      integer   :: i,j,k,ini,fin                              !
      real      :: c, x0, y0, x1, y1                          !
   !----------------------------------------------------------!
   !                           corps                          !
      surf = 0.                                               !
      do i = 1, NA                                            !
         do j = 1,2                                           !
            if ( GD(i,j) /= d ) then                          !
               cycle                                          !
            else                                              !
            endif                                             !
            !--cet arc concerne notre domaine d--             !
            c = 0.                                            !
            ini = SIF(i,1)                                    !
            x0 = float(SXY(ini,1)) ; y0 = float(SXY(ini,2))   !
            if ( PAXY(i,1) /= 0 ) then                        !
               do k = PAXY(i,1) , PAXY(i,2)                   !
                  x1 = float(AXY(k,1)); y1 = float(AXY(k,2))  !
                  c = c + (x1-x0)*(y1+y0)/2.                  !
                  x0 = x1 ; y0 = y1                           !
               enddo !--k--                                   !
            else                                              !
            endif                                             !
            fin = SIF (i,2)                                   !
            x1 = float(SXY(fin,1)) ; y1 = float(SXY(fin,2))   !
            c = c + (x1-x0)*(y1+y0)/2.                        !
            if ( j == 1 ) then                                !
                surf = surf - c                               !
            else                                              !
                surf = surf + c                               !
            endif                                             !
         enddo !--j--                                         !
      enddo !--i--                                            !
   !----------------------------------------------------------!
   end function ! -- SURFACE --                               !
   !===========================================================
                                                              !
!-------------------------------------------------------------!                                                               !
end module metrique                                           !
!MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM


module metrique_vectorielle

!***********************************************************************************************************!
!***********************************************************************************************************!
!***********************************************************************************************************!
!A REMPLIR!!!!!!!!!!!
! ce module contient des subroutine permettant des opérations vectorielle basiques sur les TABLES
! sur vecteurs decrivant la metrique d'un graphe:
!
!     SUB-ROUTINES
!     OK - getVectorTable (cf getVectorCoordinates invector.py), calcul des vecteurs a partir de la Table SIF et SXY
!     OK - GetNorme non utilisé
!     OK - GetProduit non utilisé
!     FONCTIONS
!     OK - FUNC_GetProduitScalaire
!     OK - FUNC_GetDeterminant
!     OK - FUNC_GetNorme
!     OK - FUNC_GetOrientedAngle
!
!***********************************************************************************************************!
!***********************************************************************************************************!
    use metrique                                            !
   !---------------------

contains

!***********************************************************************************************************!
    !***********************************************************************************************************!
    !SUBROUTINE
    subroutine GetVectorTable(VectorTab)
    !GetVectorTable Consomme SIF et SXYZ, retourne VectorTable
    !   -SIF :: from Table_de_graphe
    !   -SXY :: from Table_de_graphe
    !***********************************************************************************************************!
    !Specification

        integer,  intent(out),allocatable, dimension(:,:) :: VectorTab


    !***********************************************************************************************************!
    !Declaration
        !logical                         ::exist
        integer                         ::i, j
        integer                         ::SommetFinal, SommetInitial
        !character(20)                   ::str
        !character(1)                    ::sep
        !integer, dimension(3)           ::elmt

    !***********************************************************************************************************!
    !Body

    allocate (VectorTab(NA,2))

    do i = 1, NA
        do j = 1, 2
           SommetFinal = SIF(i,2)
           SommetInitial = SIF(i,1)

          VectorTab(i,j) = SXY(SommetFinal,j) - SXY(SommetInitial,j)

       end do
!           print*,"ITERATION: ",i
!           print*,"SommetFinal = SIF(i,2)",SommetFinal
!           print*,"Coordonnées X Sommet Final",SXY(SommetFinal,1)
!           print*,"Coordonnées Y Sommet Final",SXY(SommetFinal,2)
!           print*,"SommetInitial = SIF(i,1)",SommetInitial
!           print*,"Coordonnées X Sommet Initial",SXY(SommetInitial,1)
!           print*,"Coordonnées Y Sommet Initial",SXY(SommetInitial,2)
!           print*, i
!           print*, j
!           print*, "VectorTab(:,i)", VectorTab(:,i)
!
    end do

    end subroutine GetVectorTable

!***********************************************************************************************************!
    !***********************************************************************************************************!
    !SUBROUTINE
    subroutine GetNorme(a,norme)
     !GetNorme Consomme X,Y retourne Norme
        !
        !
        !***********************************************************************************************************!
        !Specification

            !integer,  intent(out),allocatable, dimension(:,:) :: VectorTab
            integer, intent(in)                                :: a
            real,   intent(out)                                :: norme

        !***********************************************************************************************************!
        !Declaration
            !logical                         ::exist
            integer                         ::i, j
            integer                         ::SommetFinal, SommetInitial
            !character(20)                   ::str
            !character(1)                    ::sep
            !integer, dimension(3)           ::elmt

        !***********************************************************************************************************!
        !Body
        norme = sqrt( float( VectorTable(a,1)*VectorTable(a,1) ) + float( VectorTable(a,2)*VectorTable(a,2) ) )

    end subroutine GetNorme




 !***********************************************************************************************************!
    !***********************************************************************************************************!
    !SUBROUTINE
    subroutine GetProduit(a,b, produitscalaire, determinant)
    !GetProduitScalaire Consomme deux arcs a et b, retourne leur produit scalaire et determinant
        !
        !
        !***********************************************************************************************************!
        !Specification

            !integer,  intent(out),allocatable, dimension(:,:) :: VectorTab
            integer, intent(in)                                :: a, b
            real,   intent(out)                                :: produitscalaire, determinant

        !***********************************************************************************************************!
        !Declaration
            !logical                        ::exist
            integer                         ::i, j

        !***********************************************************************************************************!
        !Body

        produitscalaire = float( VectorTable(1,a)*VectorTable(1,b) + VectorTable(2,a)*VectorTable(2,b) )
        determinant = float( VectorTable(1,a)*VectorTable(2,b) - VectorTable(2,a)*VectorTable(1,b) )

    end subroutine GetProduit






!***********************************************************************************************************!
!FUNCTIONS!
!***********************************************************************************************************!
    !***********************************************************************************************************!
    !FUNCTION
    real function FUNC_GetProduitScalaire(a,b) result ( produitscalaire )
    !FUNC_GetProduitScalaire Consomme deux vecteurs a et b, retourne leur produit scalaire
        ! - utilise VectorTab issue de GetVectorTable
        !
        !***********************************************************************************************************!
        !Specification

            !integer,  intent(out),allocatable, dimension(:,:) :: VectorTab
            integer, intent(in)                                :: a, b

        !***********************************************************************************************************!
        !Declaration

        !***********************************************************************************************************!
        !Body
!        print *, VectorTable(1,a), "*",VectorTable(1,b)
!        print *, VectorTable(2,a), "*",VectorTable(2,b)

        produitscalaire = float( VectorTable(a,1)*VectorTable(b,1) + VectorTable(a,2)*VectorTable(b,2) )

    end function FUNC_GetProduitScalaire


!***********************************************************************************************************!
    !***********************************************************************************************************!
    !FUNCTION
    real function FUNC_GetDeterminant(a,b) result ( Determinant )
    !FUNC_GetProduitScalaire Consomme deux vecteurs a et b, retourne leur produit scalaire
        ! - utilise VectorTab issue de GetVectorTable
        !
        !***********************************************************************************************************!
        !Specification

            !integer,  intent(out),allocatable, sdimension(:,:) :: VectorTab
            integer, intent(in)                                :: a, b

        !***********************************************************************************************************!
        !Declaration

        !***********************************************************************************************************!
        !Body
!        print *, VectorTable(1,a), "*",VectorTable(2,b)
!        print *, VectorTable(2,A), "*",VectorTable(1,b)

        determinant = float( VectorTable(a,1)*VectorTable(b,2) - VectorTable(a,2)*VectorTable(b,1) )

    end function FUNC_GetDeterminant

!***********************************************************************************************************!
    !***********************************************************************************************************!
    !FUNCTION
    real function FUNC_GetNorme(a) result (norme)
     !GetNorme Consomme X,Y retourne Norme
        !
        !
        !***********************************************************************************************************!
        !Specification

            !integer,  intent(out),allocatable, dimension(:,:) :: VectorTab
            integer, intent(in)                                :: a

        !***********************************************************************************************************!
        !Declaration

        !***********************************************************************************************************!
        !Body

        norme = sqrt( float( VectorTable(a,1)*VectorTable(a,1) ) + float( VectorTable(a,2)*VectorTable(a,2) ) )

    end function FUNC_GetNorme

!***********************************************************************************************************!
    !***********************************************************************************************************!
    !FUNCTION
    real function FUNC_OrientedAngle(a,b) result (Angle)
     !GetNorme retourne l'angle orienté entre deux vecteur a et b, sens direct, 0<angle<360
        !
        !
        !***********************************************************************************************************!
        !Specification

            !integer,  intent(out),allocatable, dimension(:,:) :: VectorTab
            integer, intent(in)                                :: a,b


        !***********************************************************************************************************!
        !Declaration
            real                                               :: determinant, normea, normeb, produitscalaire
        !***********************************************************************************************************!
        !Body

    produitscalaire = FUNC_GetProduitScalaire(a,b)
    determinant = FUNC_GetDeterminant(a,b)
    normea = FUNC_GetNorme(a)
    normeb = FUNC_GetNorme(b)

    angle = acos( produitscalaire / ( normea * normeb ) )
    angle = (angle * 180) / pi
    if ( determinant < 0 ) then
        angle = -1 * angle
    end if
    angle = 180 - angle
    print*,a,b
    print*,angle

    end function FUNC_OrientedAngle


end module metrique_vectorielle




module parcours_graphe
!***********************************************************************************************************!
!***********************************************************************************************************!
!***********************************************************************************************************!
! ce module contient des routines et fonctions implementées par le groupe pour parcourir le graphe
! graphe, ainsi que les procedures de base operant sur la
! metrique de ce graphe :
!      FONCTION
!      - FUNC_GoLeft, consomme table des prédecesseurs/successeur, retourne la val
!
! il utilise les Focntions de calculs du module metrique vectorielle.
!***********************************************************************************************************!
!***********************************************************************************************************!
    use metrique_vectorielle                                             !
   !---------------------
implicit none

contains
!***********************************************************************************************************!
    !***********************************************************************************************************!
    !SUBROUTINE
    recursive subroutine GetGD_Table(arc_ref, SommetsExplorationBool,ArcsExplorationBool,ArcsHistoriqueParcours, compteur)
    !GetVectorTable Consomme SIF et SXYZ, retourne Table GD
    !   -SIF :: from Table_de_graphe
    !   -SXY :: from Table_de_graphe
    !   Utilise ...
    !***********************************************************************************************************!
    !Specification

        !integer,  intent(out),allocatable, dimension(:,:) :: table_GD
        integer, intent(inout)                            :: arc_ref, compteur
        logical, intent(inout),  dimension(NS)            :: SommetsExplorationBool
        logical, intent(inout),  dimension(NA)            :: ArcsExplorationBool
        integer, intent(inout),  dimension (NA)           :: ArcsHistoriqueParcours


    !***********************************************************************************************************!
    !Declaration
        integer                                           :: i, j, k,  arc_a_gauche
        integer                                           :: PTCr1, PTCr2
        integer                                           :: SommetFinal, SommetInitial
        integer, allocatable, dimension (:)               :: Table_bascule, Table_des_successeurs

        !Cardinalité des Tables
        integer                                           :: nss,nta
        !character(20)                   ::str
        !character(1)                    ::sep
        !integer, dimension(3)           ::elmt

    !***********************************************************************************************************!
    !Body

    ArcsExplorationBool(arc_ref) = .true.
    ArcsHistoriqueParcours(compteur) = arc_ref

    do i=1, 2
        j = SIF(arc_ref,i)
        SommetsExplorationBool(j) = .true.
        print*,"Sommet decouvert: ",j
    end do


   ! recupere tout arc entrant ou sortant du sommet final de arc_ref.
   ! Traitement séparé Entrant et sortant
   PTCr1 = 0
   allocate( Table_des_successeurs(NA) )
   do i = 1 , 1
   allocate( Table_bascule(NA) )
        print *, "TEST PSA _ Arc ", arc_ref

        !call ESS( j, Table_bascule, nta, i)
        call PSA( arc_ref, Table_bascule, nta, 2)
        !print*,"nta: ", nta
            do k = ( PTCr1 + 1 ) , ( PTCr1 + nta )
                Table_des_successeurs(k) = Table_bascule(k)
            end do
        PTCr1 = nta
        deallocate(Table_bascule )
    end do

    !print*,Table_des_successeurs(1:PTCr1)


    arc_a_gauche = FUNC_GoLeft(arc_ref,Table_des_Successeurs,nta)


    print*,"arc à gauche: ",arc_a_gauche

    if ( ArcsExplorationBool(arc_a_gauche) .EQV. .false. ) then

        !ArcsExploration(arc_a_gauche) = .true.
        j = SIF(arc_a_gauche,2)
        SommetsExplorationBool(j) = .true.
        arc_ref = arc_a_gauche

       ! print*,compteur
        compteur = compteur + 1
        call GetGD_Table(arc_ref, SommetsExplorationBool,ArcsExplorationBool, ArcsHistoriqueParcours, compteur)

    else
        !on a trouvé une fermeture
        print*,"fermeture sur arc: ",arc_a_gauche
    end if



!1  procedure DFS(G,v):
!2      label v as discovered
!3      for all edges e in G.adjacentEdges(v) do
!4          if edge e is unexplored then
!5              w ← G.adjacentVertex(v,e)
!6              if vertex w is unexplored then
!7                  label e as a discovered edge
!8                  recursively call DFS(G,w)
!9              else
!10                 label e as a back edge
!11     label v as explored
!


    end subroutine GetGD_Table


!***********************************************************************************************************!
    !***********************************************************************************************************!
    !FUNCTION
        real function FUNC_GoLeft(vect_ref,Table_des_Successeurs,nss) result (arc_a_gauche)
             !FUNC_GoLeft retourne le numéro de l'arc "à gauche" (correspondant à l'index SIF pour parcourir le graphe
             !à la recherche de domaines.
                !   prend vect_ref en entrée, le vecteur référence
                !   parcours une table des vecteurs successeur à vect_ref
                !   REQUIERT les modules GetVectorTable, PSA
                !   REQUIERT les tables SIF, Table_des_successeurs et sa cardinalité nss (from module topologie, subroutine PSA) , SXY
                !***********************************************************************************************************!
                !Specification

                    integer, intent(in)                                :: vect_ref,nss
                    integer, intent(in), dimension (:), allocatable :: Table_des_successeurs


                !***********************************************************************************************************!
                !Declaration
                    real                                               :: angle
                    integer,  dimension (nss)                          :: Tab_angle
                    integer                                            :: i,j,k
                !***********************************************************************************************************!
                !Body

        do i = 1,nss

            k = Table_des_successeurs(i)
            angle = FUNC_OrientedAngle( vect_ref, k )
            Tab_angle(i) = angle
            if (i == 1) then
            arc_a_gauche = Table_des_successeurs(i)
            else if ( Tab_angle(i) < Tab_angle(i-1) ) then
            arc_a_gauche = Table_des_successeurs(i)
            end if

        end do

        end function FUNC_GoLeft

end module parcours_graphe


!***********************************************************************************************************!

!-----------------------------
!PROGRAM

program MAIN
!This program read the SIF and SXY file of a Graph
!-----------------------------
use tables_de_graphe
use lire_donnees
use topologie
use parcours_graphe


!Declaration
    implicit none
    character(200)          ::pathSIF, pathSXY
    integer                 ::i,j
    !Variables pour stocker les sommets et les arcs
    !Declaration Parametres formels pour Module Topologie
    integer, allocatable, dimension (:) :: TESS, TPSS, TPSA, Table_des_successeurs
    !Declaration Parametres formels pour Module Parcours de Graphe
    real                   :: norme
    logical, allocatable  ,dimension(:)            :: SommetsExplorationBool
    logical, allocatable , dimension(:)            :: ArcsExplorationBool
    integer, allocatable,  dimension (:)           :: ArcsHistoriqueParcours
    integer                                        :: Arc_ref


    integer                :: nsommet,nss,compteur

    real                   :: lgr
    !dummy variable
    integer                ::dummyInteger
!-----------------------------

!-----------------------------
!Body
    pathSIF = "./DATA/SIF.txt"
    pathSXY = "./DATA/sxy.txt"

    !Allocation table Topologie:
    !Allocation de la table SIF
    !Obtenir NA
    print *, "SUBROUTINE NUMRECORD (nbr arcs)"
    call NUMRECORD(pathSIF, NA)
    print *, "Nombre de ligne dans le fichier ", NA

    allocate(SIF(2,NA))


    !Allocation Tables Metrique:
    !Allocation de la table SXY
    !Obtenir NS

    print *, "SUBROUTINE NUMRECORD  (nbr sommets)"
    call NUMRECORD(pathSXY, NS)
    print *, "Nombre de ligne dans le fichier ", NS
    allocate(SXY(2,NS))

    !Allocation Tables Parcours Graphe:
    !Allocation de la table VectorTable
    !nombre d'éléments identiques à NA
    allocate(VectorTable(NA,2))


    !Obtenir la table SIF
    print *, "appel de la subroutine READFILE"
    call READFILE(pathSIF, SIF, NA)
    print *, "Fin de l'appel de subroutine"
    print *, "************************************************************************************"


    !Obtenir la table SXY
    print *, "appel de la subroutine READFILE"
    call READFILE(pathSXY, SXY, NS)
    print *, "Fin de l'appel de subroutine Mise en mémoire"
    print *, "************************************************************************************"

    !Obtenir la table VectorTable
    print *, "appel de la subroutine GetVectorTable"
    call GetVectorTable(VectorTable)
    print *, "Fin de l'appel de subroutine Mise en mémoire"
    print *, "************************************************************************************"

!    !Obtenir une norme + Affichage
!    print *, "appel de la subroutine GetNorme"
!    do i = 1,NA
!        call GetNorme(i,norme)
!        print*, norme
!    end do
!    print*," "
!    lgr = FUNC_GetDeterminant(1,2)
!    print*,lgr
!    lgr = FUNC_GetProduitScalaire(1,2)
!    print*,lgr
!    lgr = FUNC_GetProduitscalaire(1,1)
!    print*,lgr
!    lgr = FUNC_OrientedAngle(1,3)
!    print*,lgr


!    !Affichage de la table SIF
!    do i = 1, NA
!        print *, "Arc: ", i
!        do j = 1, 2
!            print *, SIF(i,j)
!        end do
!        print *, ""
!    end do
!    print *, "************************************************************************************"

!    !Affichage de la table SXY
!    do i = 1, NS
!        print *, "Sommet: ", i
!        do j = 1, 2
!            print *, SXY(i,j)
!        end do
!        print *, ""
!    end do
!    print *, "************************************************************************************"

!    !Affichage de la table VectorTable
!    do i = 1, NA
!            print *, "Vecteur/arc: ", i
!        do j = 1, 2
!            print *, VectorTable(j,i)
!        end do
!        print *, ""
!   end do
!
!
!    print *, "************************************************************************************"

!!***********************************************************************************************************!
!!  TEST DES MODULES TOPOLOGIE/METRIQUE BOUILLE

!    allocate(TESS(NA))
!
!    !Test ESS
!    nsommet = 0
!    print *, "TEST ESS"
!    call ESS (5, TESS, nsommet, 1 )
!    do i = 1, nsommet
!        print *, TESS(i)
!    end do
!    print *,""
!    print *, "************************************************************************************"
!    !Fin test ESS
!
!    allocate(TPSS(NA))
!
!    !Test PSS
!    nsommet = 0
!    print *, "TEST PSS"
!    call PSS (5, TPSS, nsommet, 1 )
!    print *, "nb sommets ", nsommet
!    do i = 1, nsommet
!        print *, TPSS(i)
!    end do
!    print *,""
!    print *, "************************************************************************************"
!    !Fin test PSS
!
!    allocate(Table_des_successeurs(NA))
!    !Test PSA
!
!    print *, "TEST PSA _ Arc "
!    call PSA (7, Table_des_successeurs, nss, 2 )
!    print *, "nb arcs ", nss
!    do i = 1, nss
!        print *, Table_des_successeurs(i)
!    end do
!    print *,""
!    print *, "************************************************************************************"
!!    !Fin test PSS
!
!    !test FUNC_GoLeft
!
!    dummyInteger = FUNC_GoLeft(18,Table_des_Successeurs,nss)


compteur = 1
allocate( ArcsHistoriqueParcours(NA), ArcsExplorationBool(NA), SommetsExplorationBool(NS) )
Arc_ref = 4
call GetGD_Table(Arc_ref, SommetsExplorationBool,ArcsExplorationBool, ArcsHistoriqueParcours, compteur)

print*, SommetsExplorationBool
print*,ArcsExplorationBool
print*, ArcsHistoriqueParcours

end program MAIN
!-----------------------------
