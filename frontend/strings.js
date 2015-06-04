function __(k,d) {
    var strings = {
        'saveError.title': 'Tietojen tallentaminen epäonnistui',
        'saveError.message': 'Tietojen tallentaminen epäonnistui: ',
        'userpasswordform.title' : 'Käyttäjän salasanan muuttaminen',
        'ok' : 'OK',
        'cancel' : 'Peruuta',
        'passwordMinLength' : 'Salasanan tulee olla vähintään kuusi merkkiä pitkä',
        'validationError.title' : 'Virheellisiä tietoja',
        'validationError.message' : 'Lomakkeella on virheellisiä tietoja.',
        'validationError.password' : 'Salasanat eivät täsmää.',
        'login.title' : 'Kirjautuminen Tositteet-portaaliin',
        'login.waittitle' : 'Kirjautuminen käynnissä',
        'login.waitmessage' : 'Lähetetään tietoja',
        'login.failedtitle' : 'Virhe',
        'login.failedmessage' : 'Kirjautuminen epäonnistui',
        'username' : 'Käyttäjänimi',
        'password' : 'Salasana',
        'login.login' : 'Kirjaudu sisään',
        'receipts' : 'Tositteet',
        'instructions' : 'Ohje',
        'users' : 'Käyttäjät',
        'usersgrid.title' : 'Käyttäjät',
        'usersgrid.emptyPaging' : 'Ei käyttäjiä',
        'usersgrid.new' : 'Uusi käyttäjä',
        'usersgrid.remove' : 'Poista valitut käyttäjät',
        'usergroupscombo.emptyText' : 'Käyttäjäryhmä',
        'usergroupsgrid.title' : 'Käyttäjäryhmät',
        'usergroupsgrid.emptyPaging' : 'Ei käyttäjäryhmiä',
        'usergroupsgrid.new' : 'Uusi ryhmä',
        'usergroupsgrid.remove' : 'Poista valitut ryhmät',
        'userform.title' : 'Käyttäjän tietojen muokkaaminen',
        'userform.defaultUserGroupId' : 'Oletusryhmä',
        'userform.setUserPassword' : 'Vaihda salasana',
        'search' : 'Haku',
        'name' : 'Nimi',
        'firstName' : 'Etunimi',
        'lastName' : 'Sukunimi',
        'email' : 'Sähköpostiosoite',
        'timeZone' : 'Aikavyöhyke',
        'passwordAgain' : 'Salasana uudestaan',
        'userName' : 'Käyttäjä',
        'userGroupName' : 'Käyttäjäryhmä',
        'contentType' : 'Tiedostotyyppi',
        'insertionTime' : 'Ladattu',
        'save' : 'Tallenna muutokset',
        'saveandclose' : 'Tallenna muutokset ja sulje',
        'closewithoutsaving' : 'Sulje tallentamatta muutoksia',
        'usergroupitemsgrid.title' : 'Oikeudet käyttäjäryhmissä',
        'usergroupitemsgrid.emptyPaging' : 'Ei oikeuksia käyttäjäryhmissä',
        'usergroupitemsgrid.remove' : 'Poista valitut käyttäjäoikeudet',
        'users.addReadPerm' : 'Lisää lukuoikeus valituille käyttäjille valittuihin ryhmiin',
        'users.addWritePerm' : 'Lisää luku- ja kirjoitusoikeus valituille käyttäjille valittuihin ryhmiin',
        'receiptsgrid.title' : 'Tositteet',
        'receiptsgrid.name' : 'Selite',
        'receiptsgrid.emptyPaging' : 'Ei tositteita',
        'receiptsgrid.remove' : 'Poista valitut tositteet',
        'receiptsgrid.fileName' : 'Tiedosto',
        'amount' : 'Summa',
        'insertionTime' : 'Lisätty',
        'upload.title' : 'Tiedostojen lataus',
        'upload.button' : 'Lataa tiedostoja...',
        'upload.uploading' : 'Ladataan...'
    };
    console.log(k);
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
        return __(d);
    } else {
        return k;
    }
};
