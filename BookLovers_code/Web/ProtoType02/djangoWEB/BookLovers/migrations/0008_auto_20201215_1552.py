# Generated by Django 3.0.3 on 2020-12-15 06:52

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('BookLovers', '0007_auto_20201215_1503'),
    ]

    operations = [
        migrations.AlterField(
            model_name='librarybook',
            name='addition_symbol',
            field=models.TextField(default=''),
        ),
        migrations.AlterField(
            model_name='librarybook',
            name='class_no',
            field=models.TextField(default=''),
        ),
        migrations.AlterField(
            model_name='librarybook',
            name='publication_year',
            field=models.TextField(default=''),
        ),
        migrations.AlterField(
            model_name='librarybook',
            name='vol',
            field=models.TextField(default=''),
        ),
    ]
