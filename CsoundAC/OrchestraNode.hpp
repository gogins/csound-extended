#ifndef ORCHESTRANODE_HPP_INCLUDED
#define ORCHESTRANODE_HPP_INCLUDED

class Timebase;
class Event;
class Audio;

class SILENCE_PUBLIC OrchestraNode
{
public:
    OrchestraNode();
    virtual ~OrchestraNode();
    setTimebase(Timebase *timebase);
    Timebase *getTimebase();

    virtual void addSource(OrchestraNode *orchestraNode);
    virtual size_t getSourceCount() const;
    virtual OrchestraNode *getSource(size_it index);
    virtual void setSource(size_t index, OrchestraNode *source);
    virtual void removeAllSources();


};

#endif
