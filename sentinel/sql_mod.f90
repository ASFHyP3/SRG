module sql_mod
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Module to handle string formatting between calls to 
!  Fortran and C subroutines
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer :: rc ! sql output
character :: nc=char(0) !null character


integer :: open_dbc,close_dbc,add_tblc,del_tblc,get_paramfc,get_paramdc,get_paramic,&
           get_paramcc,add_paramc,del_paramc,edit_paramcc

external :: open_dbc,close_dbc,add_tblc,del_tblc,get_paramfc,get_paramdc,get_paramic,&
            get_paramcc,add_paramc,del_paramc,edit_paramcc

contains

!!!! OPEN /CLOSE !!!!!
subroutine open_db(db,dbname)
  ! open database connection
  integer*8 :: db
  character(*) :: dbname
  character(len=len(dbname)+1+5) :: nt_dbname !null terminated, and with '/tmp/' prefix
  
  nt_dbname = '/tmp/'//trim(dbname)//nc  !add null character
!  print *,'opening database file ',nt_dbname
  rc = open_dbc(nt_dbname,db)  !open
!  print *,'rc= ',rc
end subroutine open_db


subroutine close_db(db)
  ! close database connection
  integer*8 :: db
  rc = close_dbc(db)
end subroutine close_db


!!!! ADD/REMOVE TABLE !!!!!
subroutine add_tbl(db,tbl)
  integer*8 :: db
  character(*) :: tbl
  character(len=len(tbl)+1) :: nt_tbl

  nt_tbl = trim(tbl)//nc
  rc = add_tblc(db,nt_tbl)
end subroutine add_tbl

subroutine del_tbl(db,tbl)
  integer*8 :: db
  character(*) :: tbl
  character(len=len(tbl)+1) :: nt_tbl

  nt_tbl = trim(tbl)//nc
  rc = del_tblc(db,nt_tbl)
end subroutine del_tbl

!!!! ADD/DELETE PARAMETERS !!!!!
subroutine add_param(db,tbl,name)
  integer*8 :: db
  character(*) :: tbl,name
  character(len=len(tbl)+1) :: nt_tbl
  character(len=len(name)+1) :: nt_name
  
  nt_name = trim(name)//nc
  nt_tbl = trim(tbl)//nc !add null character
  rc = add_paramc(db,nt_tbl,nt_name)
end subroutine add_param

subroutine del_param(db,tbl,name)
  integer*8 :: db
  character(*) :: tbl,name
  character(len=len(tbl)+1) :: nt_tbl
  character(len=len(name)+1) :: nt_name
  
  nt_name = trim(name)//nc
  nt_tbl = trim(tbl)//nc !add null character
  rc = del_paramc(db,nt_tbl,nt_name)
end subroutine del_param


!!!! EDIT PARAMETERS !!!!
subroutine edit_paramf(db,tbl,name,value,units,comment)
  integer*8 :: db
  character(*) :: tbl,name,units,comment
  character(len=len(tbl)+1) :: nt_tbl
  character(len=len(name)+1) :: nt_name
  character(len=len(units)+1) :: nt_units
  character(len=len(comment)+1) :: nt_comment
  character(len = 100) :: nt_value
  real*4 :: value
 
  nt_name = trim(name)//nc
  nt_tbl = trim(tbl)//nc !add null character
  nt_units = trim(units)//nc
  nt_comment = trim(comment)//nc
  write(nt_value,*) value  ! write number to string
  nt_value = trim(adjustl(nt_value))//nc

  rc = edit_paramcc(db,nt_tbl,nt_name,nt_value,nt_units,nt_comment,'REAL*4'//nc) 

end subroutine edit_paramf

subroutine edit_paramd(db,tbl,name,value,units,comment)
  integer*8 :: db
  character(*) :: tbl,name,units,comment
  character(len=len(tbl)+1) :: nt_tbl
  character(len=len(name)+1) :: nt_name
  character(len=len(units)+1) :: nt_units
  character(len=len(comment)+1) :: nt_comment
  character(len = 100) :: nt_value
  real*8 :: value
 
  nt_name = trim(name)//nc   
  nt_tbl = trim(tbl)//nc !add null character
  nt_units = trim(units)//nc
  nt_comment = trim(comment)//nc
  write(nt_value,*) value  ! write number to string
  nt_value = trim(adjustl(nt_value))//nc
  

  rc = edit_paramcc(db,nt_tbl,nt_name,nt_value,nt_units,nt_comment,'REAL*8'//nc) 

end subroutine edit_paramd

subroutine edit_parami(db,tbl,name,value,units,comment)
  integer*8 :: db
  character(*) :: tbl,name,units,comment
  character(len=len(tbl)+1) :: nt_tbl
  character(len=len(name)+1) :: nt_name
  character(len=len(units)+1) :: nt_units
  character(len=len(comment)+1) :: nt_comment
  character(len = 100) :: nt_value
  integer*4 :: value
 
  nt_name = trim(name)//nc
  nt_tbl = trim(tbl)//nc !add null character
  nt_units = trim(units)//nc
  nt_comment = trim(comment)//nc
  write(nt_value,*) value  ! write number to string
  nt_value = trim(adjustl(nt_value))//nc

  rc = edit_paramcc(db,nt_tbl,nt_name,nt_value,nt_units,nt_comment,'INT*4'//nc) 

end subroutine edit_parami

subroutine edit_paramc(db,tbl,name,value,units,comment)
  integer*8 :: db
  character(*) :: tbl,name,units,comment,value
  character(len=len(tbl)+1) :: nt_tbl
  character(len=len(name)+1) :: nt_name
  character(len=len(units)+1) :: nt_units
  character(len=len(comment)+1) :: nt_comment
  character(len=len(value)+1) :: nt_value

 
  nt_name = trim(name)//nc
  nt_tbl = trim(tbl)//nc !add null character
  nt_units = trim(units)//nc
  nt_comment = trim(comment)//nc
  nt_value = trim(adjustl(value))//nc
 

  rc = edit_paramcc(db,nt_tbl,nt_name,nt_value,nt_units,nt_comment,'CHAR*'//nc) 

end subroutine edit_paramc

!!!! GET PARAMETERS !!!!!
subroutine get_paramf(db,tbl,name,value,units,type)
  integer*8 :: db
  character(*) :: tbl,units,type,name
  real*4 :: value
  character(len=len(tbl)+1) :: nt_tbl
  character(len=len(name)+1) :: nt_name
  character(len=len(units)) :: units_tmp
  character(len=len(type))  :: type_tmp
  integer*4 :: unit_len,type_len

  unit_len = len(units)
  type_len = len(type)
  nt_tbl = trim(tbl)//nc
  nt_name = trim(name)//nc
  rc = get_paramfc(db,nt_tbl,nt_name,value,units_tmp,type_tmp,&
       unit_len,type_len)
  units = units_tmp(1:unit_len)
  type = type_tmp(1:type_len)
end subroutine get_paramf

subroutine get_paramd(db,tbl,name,value,units,type)
  integer*8 :: db
  character(*) :: tbl,units,type,name
  real*8 :: value
  character(len=len(tbl)+1) :: nt_tbl
  character(len=len(name)+1) :: nt_name
  character(len=len(units)) :: units_tmp
  character(len=len(type))  :: type_tmp
  integer*4 :: unit_len,type_len

  unit_len = len(units)
  type_len = len(type)
  nt_tbl = trim(tbl)//nc
  nt_name = trim(name)//nc
  rc = get_paramdc(db,nt_tbl,nt_name,value,units_tmp,type_tmp,&
       unit_len,type_len)
  units = units_tmp(1:unit_len)
  type = type_tmp(1:type_len)
end subroutine get_paramd

subroutine get_parami(db,tbl,name,value,units,type)
  integer*8 :: db
  character(*) :: tbl,units,type,name
  integer*4 :: value
  character(len=len(tbl)+1) :: nt_tbl
  character(len=len(name)+1) :: nt_name
  character(len=len(units)) :: units_tmp
  character(len=len(type))  :: type_tmp
  integer*4 :: unit_len,type_len

  unit_len = len(units)
  type_len = len(type)
  nt_tbl = trim(tbl)//nc
  nt_name = trim(name)//nc
  rc = get_paramic(db,nt_tbl,nt_name,value,units_tmp,type_tmp,&
       unit_len,type_len)
  units = units_tmp(1:unit_len)
  type = type_tmp(1:type_len)
end subroutine get_parami

subroutine get_paramc(db,tbl,name,value,units,type)
  integer*8 :: db
  character(*) :: tbl,units,type,name,value
  character(len=len(tbl)+1) :: nt_tbl
  character(len=len(name)+1) :: nt_name
  character(len=len(units)) :: units_tmp
  character(len=len(type))  :: type_tmp
  character(len=len(value)) :: val_tmp
  integer*4 :: unit_len,type_len,val_len

  unit_len = len(units)
  val_len = len(value)
  type_len = len(type)
  nt_tbl = trim(tbl)//nc  !null terminate
  nt_name = trim(name)//nc
  rc = get_paramcc(db,nt_tbl,nt_name,val_tmp,units_tmp,type_tmp,&
       val_len,unit_len,type_len)
  units = units_tmp(1:unit_len)
  type = type_tmp(1:type_len)
  value = val_tmp(1:val_len)
end subroutine get_paramc



end module sql_mod
