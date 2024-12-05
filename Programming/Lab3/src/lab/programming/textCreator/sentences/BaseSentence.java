package lab.programming.textCreator.sentences;

import java.util.ArrayList;
import lab.programming.textCreator.words.SentenceMember;
import lab.programming.textCreator.words.Subject;
import lab.programming.textCreator.words.SubjectBuilder;
import lab.programming.textCreator.words.MultipleMember;
import lab.programming.textCreator.words.Predicate;

public class BaseSentence {
    ArrayList<SentenceMember> members = new ArrayList<SentenceMember>();
    
    BaseSentence(){}
    BaseSentence(SentenceBuider buider) {
        this.members = buider.members;
    }

    void addMember(SentenceMember member) {
        members.add(member);
    }
    
    @Override
    public String toString() {
        return "";
    }

    private static class SentenceBuider {
        ArrayList<SentenceMember> members = new ArrayList<SentenceMember>();

        SentenceBuider addPredicate(Predicate pred) {
            members.add(pred);
            return this;
        }
        
        SentenceBuider addSBuider(Subject sub) {
            members.add(new SubjectBuilder(sub).build());
            return this;
        }

        SentenceBuider addMultiple(MultipleMember mult){
            members.add(mult);
            return this;
        }

        BaseSentence build() {
            BaseSentence sent = new BaseSentence(this);
            return sent;
        }
    }
}
