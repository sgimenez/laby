def look():
    "Expected responses from look(): void, wall, rock, web, exit."
    response = input("look\n")
    say(response)
    if response == "quit":
        exit(0)
    return response


def left():
    return input("left\n")


def right():
    return input("right\n")


def forward():
    return input("forward\n")


def take():
    return input("take\n")


def drop():
    return input("drop\n")


def escape():
    return input("escape\n")


def say(s):
    "Prompt string is returned and displays in the Messages panel."
    input("say " + s + "\n")


input("start\n")
