class HelloContextManager:
    def __enter__(self):
        print("Entering the context...")
        return "Hello, World!"
    def __exit__(self, exc_type, exc_value, exc_tb):
        print("Leaving the context...")
        print(exc_type, exc_value, exc_tb, sep="\n")


with HelloContextManager() as hello:
    print(hello)
    with HelloContextManager() as hello2:
        print(hello2)