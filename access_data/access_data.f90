!***********************************************************************************************************!
!***********************************************************************************************************!
!***********************************************************************************************************!
!MODULE ACCESS_DATA
!Ce module contient les fonctions nécessaires pour récupérer les données stockées dans les tables:
!SIF, SXY, PAXY
!   -NUMRECORD, retourne le nombre d'enregistrements des différents fichiers
!   -READFILE, lit les fichiers et retourne un tableau de données, ainsi que le nombre d'enregistrements
!   -SPLIT, ce charge de séparer une ligne du fichier en élément indicé.
!***********************************************************************************************************!

module access_data

implicit none
contains

!***********************************************************************************************************!
!***********************************************************************************************************!
!SUBROUTINE
subroutine READFILE(pathFile, tab, n)
!READFILE, lit les fichiers et retourne un tableau de données, ainsi que le nombre d'enregistrements
!   -pathFile :: chemins d'accès au fichier
!   -tab :: tableau d'élémént retourné par le fichier
!   -n :: nombre d'éléments contenu dans le tableau
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
    allocate(tab(n,3))
    !Vérification de l'existence du fichier
    inquire(file=pathFile,exist= exist)
    if (.NOT. exist) then
        print *, "Erreur! Fichier non existant"
        return
    endif

    !Ouverture du fichier
    open(10, file=pathFile)
    !Lecture du fichier sur n lignes (on les a récupéré avec NUMRECORD
    do i = 1, n
        read(10,1000,iostat=io)str
        !Test de fin de fichier
        if(io < 0) exit
        sep = ";"
        !Récupération des éléments contenus dans la chaîne
        call SPLIT(str, sep, elmt, nelmt)

        do j = 1, nelmt
            tab(i,j) = elmt(j)
            end do
    enddo
    !Fermeture du fichier
    close(10)

deallocate (tab)
1000 FORMAT(a10)
end subroutine READFILE

!***********************************************************************************************************!
!***********************************************************************************************************!
!SUBROUTINE
subroutine NUMRECORD(pathFile, n)
!READFILE, lit les fichiers et retourne un tableau de données, ainsi que le nombre d'enregistrements

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
!SPLIT, ce charge de séparer une ligne du fichier en élément indicé.
!   -str :: la chaîne de caractère à traiter
!   -sep :: le séparateur d'éléments
!   - T :: le tableau d'élément qui sera retourné. La taille peut-être variable (XY ou XYZ, etc.)
!   - n :: le nombred'élément dans le tabeau.

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
            !Convertir la chaîne de caractère en entier
            read(str(pos1:), *) val
            T(n) = val
            exit
        end if
        n = n + 1
        !Convertir la chaîne de caractère en entier
        read(str(pos1:pos1 + pos2 - 2), *) val
        T(n) = val
        pos1 = pos1 + pos2
    end do

end subroutine SPLIT

end module access_data

!-----------------------------
!PROGRAM

program MAIN
!This program read the SIF and SXY file of a Graph
!-----------------------------
use access_data
!Declaration
    implicit none
    character(200)          ::path
    integer,allocatable, dimension(:,:)      ::tab
    integer                 ::n
!-----------------------------

!-----------------------------
!Body
    path = "E:\Python\graphe\SIFF.txt"
    print *, "SUBROUTINE NUMRECORD",
    call NUMRECORD(path, n)
    print *, "Nombre de ligne dans le fichier ", n
    !allocate(tab(n,n,n))

    print *, "appel de la subroutine READFILE"
    call READFILE(pathUncleaned, tab, n)
    print *, "Fin de l'appel de subroutine"

end program MAIN
!-----------------------------
