
all: app release

app:
	make -C lib/drink-1.0

release: drink.rel
	erlc -W drink.rel

clean:
	make -C lib/drink-1.0 clean
	rm -f drink.

run:
	echo "Testing"
