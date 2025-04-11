
all:\
  make_lib\
  make_common1\
  make_common2\
  make_common3\
  make_main_std\
  make_main_ext\
  make_libspring\
	make_sample_libspring

make_lib:
	$(MAKE) -C lib

make_common1:
	$(MAKE) -C common1

make_common2:
	$(MAKE) -C common2

make_common3:
	$(MAKE) -C common3

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
  clean_common1\
  clean_common2\
  clean_common3\
  clean_main_std\
  clean_main_ext\
	clean_libspring\
	clean_sample_libspring


clean_lib:
	$(MAKE) -C lib clean

clean_common1:
	$(MAKE) -C common1 clean

clean_common2:
	$(MAKE) -C common2 clean

clean_common3:
	$(MAKE) -C common3 clean

clean_main_std:
	$(MAKE) -C main_std clean

clean_main_ext:
	$(MAKE) -C main_ext clean

clean_libspring:
	$(MAKE) -C libspring clean

clean_sample_libspring:
	$(MAKE) -C sample_libspring clean
