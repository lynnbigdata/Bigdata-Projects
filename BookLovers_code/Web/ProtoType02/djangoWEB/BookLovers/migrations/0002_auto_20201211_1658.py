# Generated by Django 3.0.3 on 2020-12-11 07:58

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('BookLovers', '0001_initial'),
    ]

    operations = [
        migrations.AddField(
            model_name='healingbook',
            name='negative_score',
            field=models.FloatField(default=0),
        ),
        migrations.AddField(
            model_name='healingbook',
            name='positive_score',
            field=models.FloatField(default=0),
        ),
    ]
