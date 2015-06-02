function __(k,d) {
    var strings = {
        'login/title' : 'Kirjautuminen Tositteet-portaaliin',
        'login/waittitle' : 'Kirjautuminen käynnissä',
        'login/waitmessage' : 'Lähetetään tietoja',
        'login/failedtitle' : 'Virhe',
        'login/failedmessage' : 'Kirjautuminen epäonnistui',
        'login/username' : 'Käyttäjänimi',
        'login/password' : 'Salasana',
        'login/login' : 'Kirjaudu sisään'
    };
    var parts = k.split('.');
    if (k in strings) {
        while (k in strings) {
            var pk = k;
            k = strings[k];
            if (pk == k)
                break;
        }
        return k;
    } else if (parts[parts.length - 1] in strings) {
        k = parts[parts.length - 1];
        while (k in strings) {
            var pk = k;
            k = strings[k];
            if (pk == k)
                break;
        }
        return k;
    } else if (d) {
        return translate(d);
    } else {
        return k;
    }
};
