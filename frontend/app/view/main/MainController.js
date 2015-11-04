Ext.define('SMS.view.main.MainController', {
    extend: 'Ext.app.ViewController',

    requires: [
        'Ext.window.MessageBox'
    ],

    alias: 'controller.main',
    config: {
        control: {
            '#maintab' : {
                tabchange: 'onTabChange'
            }
        },
        routes : {
            'maintab:id' : {
                action     : 'showTab',
                conditions : {
                    ':id'    : '(?:(?::){1}([%a-zA-Z0-9\-\_\s,]+))?'
                }
            }
        }
    },
   onLogin: function() {
        
        if (SMS.GlobalState.user.config.usersTab == true) {
            this.lookupReference('usersTab').tab.show();
        }

        this.redirectTo('maintab:maintab-clients');
    },

    addUserGroupItems: function(userGrid, userGroupGrid, userGroupItemsGrid, mode) {
        var users = userGrid.getSelectionModel().getSelection(),
            userGroups = userGroupGrid.getSelectionModel().getSelection(),
            userGroupItems = userGroupItemsGrid.store;
        for (var i = 0; i < users.length; i++) {
            for (var i2 = 0; i2 < userGroups.length; i2++) {
                (function(user, userGroup) {
                    userGroupItems.add(Ext.create('SMS.model.usergroupitems',
                            {
                            userId: user.getId(),
                            userName: user.getData()['name'],
                           userGroupId : userGroup.getId(),
                            userGroupName: userGroup.getData()['name'],
                          mode: mode }));
                 })(users[i], userGroups[i2]);
            }
        }
        userGroupItems.sync();
    },
    onTabChange: function(tabPanel, newItem) {
        var id = newItem.getId();
        this.redirectTo('maintab:' + id);
    },
    showTab : function(id) {    
        Ext.WindowManager.each(function(cmp) { cmp.destroy(); });
        var tabPanel = this.lookupReference('mainTab');
        if (!id) {
            id = 0;
        }
        var child = tabPanel.getComponent(id);
        tabPanel.setActiveTab(child);
    },
    init: function() {
        var controller = this;
        SMS.GlobalState.on('login', function() {
            controller.onLogin();
            
        });

        this.control({
           'panel[name=users] button[name=addReadPerm]': {
                click: function(button) {
                    var panel = button.up('panel[name=users]');
                    controller.addUserGroupItems(panel.down('usersgrid'),
                                                 panel.down('usergroupsgrid'),
                                                 panel.down('usergroupitemsgrid'),
                                                 'ReadOnly');

                }
            },
            'panel[name=users] button[name=addWritePerm]': {
                click: function(button) {
                    var panel = button.up('panel[name=users]');
                    controller.addUserGroupItems(panel.down('usersgrid'),
                                                 panel.down('usergroupsgrid'),
                                                 panel.down('usergroupitemsgrid'),
                                                 'ReadWrite');
                }
            },
            'button[name=logout]' : {
                click: function(button) {
                    $.ajax({
                        url:'backend/auth/logout',
                        type:'POST',
                        dataType:'json'
                    }).always(function() {
                        location.reload();
                    });
                }
            },
            'clientsgrid button[name=export]' : {
                click: function(button) {
                    $("body").append('<iframe src="backend/clients/xlsx" style="display: none;" ></iframe>');
                }
            },
            'clientsgrid button[name=sendMessage]' : {
                click: function(button) {
                    var grid = button.up('grid'),
                        textSearch = grid.down('textfield[itemId=textSearch]')
                        monthCombo = grid.down('monthcombo');
                    $.ajax({
                        url:'backend/db/targetedtextmessage',
                        type:'POST',
                        dataType:'json',
                        data: JSON.stringify({
                            text: '',
                            query: textSearch.getValue() ? textSearch.getValue() : null,
                            dateOfBirthMonth: monthCombo.getValue() ? monthCombo.getValue() : null
                        })
                    }).done(function(data) {
                         controller.redirectTo('maintab:maintab-textmessages');
                         Ext.getStore('textmessages').load(
                            {
                                callback: function() {
                                    setTimeout(function() {

                                        var record = Ext.getStore('textmessages').getById(data.id);
                                         
                                        window.yesodDsl.openFormWindow('textmessageform', 610, 630, record);
                                    }, 500);
                                }
                            }
                         );
                     });
                }
            }      
        });
    }

});
