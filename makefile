default: main
	./Main script

main: Main.hs Parse.hs Transform.hs Line.hs Solids.hs Screen.hs DrawMats.hs \
		Lighting.hs Lexer.hs
	ghc -dynamic -O2 Main.hs

Lexer.hs: mdl.x
	alex -o Lexer.hs -g mdl.x

clean:
	rm *.hi *.o Lexer.hs Main .tempimg.ppm

imgclean:
	rm *.ppm *.png .tempimg.ppm

run:
	./Main script
