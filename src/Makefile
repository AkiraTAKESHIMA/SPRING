
all:\
  make_lib\
  make_cmn1\
  make_cmn2\
  make_cmn3\
  make_main_std\
  make_main_ext\
  make_libspring\
	make_sample_libspring

make_lib:
	$(MAKE) -C lib

make_cmn1:
	$(MAKE) -C cmn1

make_cmn2:
	$(MAKE) -C cmn2

make_cmn3:
	$(MAKE) -C cmn3

make_main_std:
	$(MAKE) -C main_std

make_main_ext:
	$(MAKE) -C main_ext

make_libspring:
	$(MAKE) -C libspring

make_sample_libspring:
	$(MAKE) -C sample_libspring



install:\
  install_main_std\
  install_main_ext

install_main_std:
	$(MAKE) -C main_std install

install_main_ext:
	$(MAKE) -C main_ext install


clean:\
  clean_lib\
  clean_cmn1\
  clean_cmn2\
  clean_cmn3\
  clean_main_std\
  clean_main_ext\
	clean_libspring\
	clean_sample_libspring


clean_lib:
	$(MAKE) -C lib clean

clean_cmn1:
	$(MAKE) -C cmn1 clean

clean_cmn2:
	$(MAKE) -C cmn2 clean

clean_cmn3:
	$(MAKE) -C cmn3 clean

clean_main_std:
	$(MAKE) -C main_std clean

clean_main_ext:
	$(MAKE) -C main_ext clean

clean_libspring:
	$(MAKE) -C libspring clean

clean_sample_libspring:
	$(MAKE) -C sample_libspring clean
