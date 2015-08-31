(function() {
    var months = Ext.create('Ext.data.Store', {
            fields: ['id', 'name'],
            data : [{ "id" : '__EMPTY_VALUE__', "name" : "---" }].concat(_.map(_.range(1,13), function(v) {
                    return {
                        "id" : v,
                        "name" : __("month" + v)
                    };
                }))
        });

    Ext.define('SMS.view.MonthCombo',{
        extend: 'Ext.form.field.ComboBox',
        alias: 'widget.monthcombo',
        store: months,  
        editable:false,
        emptyText: __('emptyMonthCombo'),
        valueField : 'id',
        displayField : 'name',
        listeners : {
            change: function(combo, newValue, oldValue, eOpts ) {
                if (newValue == '__EMPTY_VALUE__') {
                    combo.setRawValue('');
                }
            }
        }
    });
})();
