
#include <stdio.h>
#include <string>
#include "RaftConsensus.h"
#include "StateMachine.h"

using namespace LogCabin;

#define UNUSED __attribute__((unused))

int main(int argc, char *argv[])
{
    Core::Config config;

    std::shared_ptr<Server::RaftConsensus> raft;
    std::shared_ptr<Server::StateMachine> stateMachine;

    raft.reset(new Server::RaftConsensus(config));
    raft->init();

    stateMachine.reset(new Server::StateMachine(raft, config));

    //typedef Server::RaftConsensus::ClientResult Result;
    //std::string operation;
    //UNUSED std::pair<Result, uint64_t> result = raft->replicate(operation);

    return 0;
}


