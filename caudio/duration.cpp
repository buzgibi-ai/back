#include "AudioFile.h"
#include <iostream>

int main(int argc, char* argv[])
{
    if (argv[1])
    {
        const std::string path = (std::string) argv[1];
        AudioFile<float> a;
        a.shouldLogErrorsToConsole(false);
        a.load((std::string) argv[1]);
        std::cout << a.getLengthInSeconds();
    } else { std::cout << (double) 0.0; }
}
