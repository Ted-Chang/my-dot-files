.PHONY: install help

install:
	install -m 0644 -b .bashrc ~/.bashrc
	install -m 0644 -b etc/profile.d/prompt.sh /etc/profile.d/prompt.sh
	install -m 0644 .emacs ~/.emacs
	cp -rf .emacs.d ~/
	install -m 0644 .gdbinit ~/.gdbinit
	install -m 0644 .gitconfig ~/.gitconfig
	install -m 0644 .gitignore_global ~/.gitignore_global
	install -m 0644 .valgrindrc ~/.valgrindrc

help:
	@echo "Avaiable make targets:"
	@echo
	@echo "install - install the start up files"
	@echo "help    - print this help info"
