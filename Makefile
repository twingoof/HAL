##
## EPITECH PROJECT, 2021
## evalExpr
## File description:
## Makefile
##

NAME	=	hal

BINPATH	=	$(shell stack path --local-install-root)

CP		=	cp

all: $(NAME)

$(NAME):
	stack build
	$(CP) $(BINPATH)/bin/$(NAME)-exe ./$(NAME)

clean:
	stack clean

fclean: clean
	$(RM) $(NAME)

tests_run:
	stack test

re: fclean all

dbg:
	stack ghci

.PHONY: all clean fclean tests_run re dbg