##
## EPITECH PROJECT, 2021
## evalExpr
## File description:
## Makefile
##

NAME	=	hal

BINPATH	=	$(shell stack path --local-install-root)

COVPATH	=	$(shell stack path --local-hpc-root)

COVDIR	=	.coverage

CP		=	cp

all: $(NAME)

$(NAME):
	stack build
	$(CP) $(BINPATH)/bin/$(NAME)-exe ./$(NAME)

clean:
	stack clean

fclean: clean
	$(RM) $(NAME)
	$(RM) -r $(COVDIR)

tests_run:
	stack test --coverage
	$(CP) -r $(COVPATH) ./$(COVDIR)

re: fclean all

dbg:
	stack ghci

.PHONY: all clean fclean tests_run re dbg