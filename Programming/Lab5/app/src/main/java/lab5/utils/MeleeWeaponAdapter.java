package lab5.utils;

import jakarta.xml.bind.annotation.adapters.XmlAdapter;
import lab5.spacemarines.MeleeWeapon;

public class MeleeWeaponAdapter extends XmlAdapter<String, MeleeWeapon> {

    @Override
    public MeleeWeapon unmarshal(String v) throws Exception {
        return MeleeWeapon.valueOf(v);
    }

    @Override
    public String marshal(MeleeWeapon v) throws Exception {
        return v.name();
    }
}
