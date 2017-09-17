all: cli webserver

cli:
	jbuilder build src/logarion_cli.exe

webserver:
	jbuilder build src/logarion_webserver.exe

clean:
	jbuilder clean

theme-dark:
	sassc share/sass/main-dark.sass > share/static/main.css

theme-light:
	sassc share/sass/main-light.sass > share/static/main.css
