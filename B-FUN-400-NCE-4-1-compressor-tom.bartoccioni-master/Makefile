##
## EPITECH PROJECT, 2019
## Compressor
## File description:
## Makefile
##

BIN = `stack path --local-install-root`/bin/Compressor-exe

NAME =		imageCompressor

all:		$(NAME)

$(NAME): $(BIN)
		cp $(BIN) ./$(NAME)

$(BIN): ./app/*.hs
		stack build

clean:
		stack clean

fclean:		clean
		rm -f $(NAME)

re:		fclean all