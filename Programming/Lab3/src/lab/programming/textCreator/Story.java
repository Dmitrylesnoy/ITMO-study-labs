package lab.programming.textCreator;

import lab.programming.textCreator.sentences.BaseSentence;
import lab.programming.textCreator.words.*;

public class Story {
    public static void main(String[] args) {

        Subject car = new Subject("автомобиль", "");
        SubjectBuilder carB = new SubjectBuilder(car);

        Predicate predicate1 = new Predicate("выехал");
        predicate1.setPlace(new Subject("заросли", "маковые"), "из");
        predicate1.addSubject(carB.build());

        Subject travelers = new Subject("путешественники", "наши");
        Predicate predicate2 = new Predicate("увидали");
        Subject strangeMachine = new Subject("машина", "странная");

        predicate2.addSubject(new SubjectBuilder(travelers));
        predicate2.setPlace(new Subject("дорога", "недалекая"), "от");
        predicate2.setDefenition(strangeMachine.toString());

        Predicate carDescrip1 = new Predicate("напоминавшую", "", Speed.NOMOVE);
        carDescrip1.addSubject(new SubjectBuilder(strangeMachine));
        Subject strMachDescrip1 = new Subject("чистка", "механическая");
        Subject strMachDescrip2 = new Subject("трактор", "");

        MultipleMember multyMember1 = new MultipleMember("и");
        multyMember1.addSubject(predicate1);
        multyMember1.addSubject(predicate2);
        MultipleMember multyMember2 = new MultipleMember("не то");
        multyMember2.addSubject(carDescrip1);
        multyMember2.addSubject(new SubjectBuilder(strMachDescrip1));
        multyMember2.addSubject(new SubjectBuilder(strMachDescrip2));

        BaseSentence sent1 = new BaseSentence.SentenceBuider().addMultiple(multyMember1).addMultiple(multyMember2)
                .build();

        write(sent1.toString());

        Subject snowCleaner = new Subject("снегочистка", "эта");
        Predicate snowPredicate1 = new Predicate("ходила", "", Speed.SLOW);
        snowPredicate1.setPlace(new Subject("круг", ""), "по");
        Predicate snowPredicate2 = new Predicate("косила");
        snowPredicate2.addSubject(new SubjectBuilder("траву", "").build());
        MultipleMember multSnow = new MultipleMember();
        multSnow.setKnot("и");
        multSnow.addSubject(snowPredicate1);
        multSnow.addSubject(snowPredicate2);
        BaseSentence sent2 = new BaseSentence.SentenceBuider().addSubject(snowCleaner).addMultiple(multSnow).build();
        write(sent2.toString());

        Subject neznayka = new Subject("Незнайка", "");

        Predicate nezPredicate1 = new Predicate("остановил", car.toString(), Speed.NOMOVE);
        nezPredicate1.addSubject(new SubjectBuilder(neznayka).build());

        Predicate nezPredicate2 = new Predicate("посмотреть", "как", Speed.NOMOVE);
        nezPredicate2.setPlace(car, "");
        MultipleMember nezMult1 = new MultipleMember("чтоб");
        nezMult1.addSubject(nezPredicate1);
        MultipleMember nezMult2 = new MultipleMember("как");
        Predicate nezPredicate3 = new Predicate("работает", "", Speed.NOMOVE);
        nezPredicate3.addSubject(carB.build());
        nezMult2.addSubject(nezPredicate3);
        nezMult1.addSubject(nezPredicate2);

        BaseSentence sent3 = new BaseSentence.SentenceBuider().addMultiple(nezMult1).addMultiple(nezMult2).build();
        write(sent3.toString());

        MultipleMember mechMult = new MultipleMember();
        mechMult.addSubject(new Predicate("подойдя", "ближе", Speed.NOMOVE));
        Predicate mechPredicate1 = new Predicate("увидели");
        mechPredicate1.addSubject(new SubjectBuilder("путники", "наши").build());
        mechMult.addSubject(mechPredicate1);

        Subject mechanizm = new Subject("механизм", "");
        Predicate mechPredicate2 = new Predicate("был");
        mechPredicate2.setPlace(new Subject("передняя часть " + car.toString(), ""), "в");
        mechPredicate2.addSubject(new SubjectBuilder(mechanizm).build());

        MultipleMember mechMult2 = new MultipleMember("что");
        mechMult2.addSubject(mechMult);
        mechMult2.addSubject(mechPredicate2);

        Predicate mechPredicate3 = new Predicate("напонимал");
        Subject mechCut = new Subject("машинка", "для стрижки волос");
        SubjectBuilder mechCutB = new SubjectBuilder(mechCut);
        mechPredicate3.addSubject(mechCutB.build());
        BaseSentence sent4 = new BaseSentence.SentenceBuider().addMultiple(mechMult2).addPredicate(mechPredicate3)
                .build();

        write(sent4.toString());

        Subject grass = new Subject("трава", "");
        SubjectBuilder grassB = new SubjectBuilder(grass);
        Predicate cutPredicate1 = new Predicate("стригла", grass.toString(), Speed.ALWAYS);
        cutPredicate1.addSubject(mechCutB.build());
        Predicate cutPredicate2 = new Predicate("попадала");

        Subject knife = new Subject("нож", "этот");
        cutPredicate2.setPlace(knife, "под");
        cutPredicate2.addSubject(grassB.build());

        MultipleMember cutMult = new MultipleMember();
        cutMult.addSubject(cutPredicate1);
        cutMult.addSubject(cutPredicate2);
        BaseSentence sent5 = new BaseSentence.SentenceBuider().addMultiple(cutMult).build();
        write(sent5.toString());

        Predicate knifePredicate1 = new Predicate("кромсал", grass.toString() + " на кусочки", Speed.ALWAYS);
        knifePredicate1.addSubject(new SubjectBuilder(knife).build());

        MultipleMember knifeMult = new MultipleMember("после чего");
        Predicate knifePredicate2 = new Predicate("поступала");
        knifePredicate2.setPlace(new Subject("лента", ""), "на");
        knifePredicate2.addSubject(grassB.build());
        knifeMult.addSubject(knifePredicate2);

        MultipleMember knifeMult2 = new MultipleMember("и");
        knifeMult2.addSubject(new Predicate("уносилась", grass));
        Predicate knifPredicate3 = new Predicate("попадала");
        Subject barabans = new Subject("барабаны", "зубчатые");
        knifPredicate3.setPlace(barabans, "между");

        MultipleMember knifeMult4 = new MultipleMember("и");
        Predicate eatTooth = new Predicate("жевали", grass.toString() + " зубами", Speed.NOMOVE);
        eatTooth.addSubject(new SubjectBuilder(barabans).build());
        Predicate circle = new Predicate("вращались", "", Speed.FAST);
        circle.addSubject(new SubjectBuilder(barabans).build());
        knifeMult4.addSubject(eatTooth);
        knifeMult4.addSubject(circle);

        knifeMult2.addSubject(knifPredicate3);

        BaseSentence sent6 = new BaseSentence.SentenceBuider().addPredicate(knifePredicate1).addMultiple(knifeMult)
                .addMultiple(knifeMult2).addMultiple(knifeMult4).build();
        write(sent6.toString());

        Predicate hide = new Predicate("исчезала", grass);
        hide.setDefenition("таким образом");
        hide.setPlace(mechanizm, "внутри");
        BaseSentence sent7 = new BaseSentence.SentenceBuider().addPredicate(hide).build();
        write(sent7.toString());

    }

    private static void write(String s) {
        System.out.print(s);
    }

    private static void writeln(String s) {
        System.out.println(s);
    }
}
